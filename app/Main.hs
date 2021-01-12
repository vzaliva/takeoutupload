{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.List
import           Mbox
import           System.Console.GetOpt
import           System.Environment         (getArgs, getProgName)

data Options = Options
    { optVerbose :: Bool
    , optDryRun  :: Bool
    , optConfig  :: String
    , optSkip    :: Int
    , optLimit   :: Maybe Int
    } deriving Show

defaultOptions = Options
    { optVerbose     = False
    , optDryRun      = False
    , optConfig      = "takeoutupload.cfg"
    , optSkip        = 0
    , optLimit       = Nothing
    }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> Right opts { optVerbose = True }))
        "chatty output on stderr"
    , Option ['d']     ["dry-run"]
        (NoArg (\ opts -> Right opts { optDryRun = True }))
        "verobose progress on stderr"
    , Option ['c']     ["config"]
        (ReqArg (\ d opts -> Right opts { optConfig = d }) "FILE")
        "config file"
    , Option ['s'] ["skip"]
      (ReqArg (\n opts ->
        case reads n of
          [(n, "")] | n >= 0 -> Right opts { optSkip = n }
          _ -> Left "--skip value must be an non-negative integer"
        ) "number")
      "number of messages to skip"
    , Option ['l'] ["limit"]
      (ReqArg (\n opts ->
        case reads n of
          [(n, "")] | n >= 1 -> Right opts { optLimit = Just n }
          _ -> Left "--limit value must be an positive integer"
        ) "number")
      "number of messages to process"
    ]

parseOpts :: [String] -> IO (Options, String)
parseOpts argv =
    do
      progName <- getProgName
      let helpMessage = "Usage: " ++ progName ++ " [OPTION...] <inputfile>"
      case getOpt Permute options argv of
        (o,[n],[]) ->
            case foldM (flip id) defaultOptions o of
              Right opts -> return (opts,n)
              Left errorMessage -> ioError (userError (errorMessage ++ "\n" ++ helpMessage))
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo helpMessage options))

-- Per RFC 2822
-- "Unfolding is accomplished by simply removing any CRLF
--  that is immediately followed by WSP."
unfoldHeader :: LB.ByteString -> LB.ByteString
unfoldHeader (LB.uncons -> Nothing) = LB.empty
unfoldHeader (LB.uncons -> Just ('\r', LB.uncons -> Just ('\n', LB.uncons -> Just(' ',xs)))) = LB.cons ' ' (unfoldHeader xs)
unfoldHeader (LB.uncons -> Just ('\r', LB.uncons -> Just ('\n', LB.uncons -> Just('\t',xs)))) = LB.cons '\t' (unfoldHeader xs)
unfoldHeader (LB.uncons -> Just ('\r', xs)) = unfoldHeader xs
unfoldHeader (LB.uncons -> Just (x, xs)) = LB.cons x (unfoldHeader xs)

splitAtChar :: Char -> LB.ByteString -> Maybe (LB.ByteString, LB.ByteString)
splitAtChar c s = do
  i <- LB.elemIndex c s
  let (n,v) = (LB.splitAt i s)
  return (n, LB.tail v)

splitHeader :: LB.ByteString -> Maybe [(LB.ByteString, LB.ByteString)]
splitHeader = sequence . map (splitAtChar ':') . LB.lines

main :: IO ()
main =
    do
      (opts, inputfile) <- getArgs >>= parseOpts
      msgs <- processMBFile inputfile
      let msgs' = drop (optSkip opts) msgs in
        let msgs'' = case optLimit opts of
                     Nothing -> msgs'
                     Just n  -> take n msgs' in
          do
            putStrLn "Hi!" ;;
            ;; putStrLn (show (optSkip opts))
            ;; putStrLn (show (optLimit opts))
            ;; putStrLn (show (length msgs''))
            ;; mapM_ (putStrLn . LB.unpack . fromLine) msgs''
            ;; mapM_ (putStrLn . show . splitHeader . unfoldHeader . headers) msgs''
--            ;; mapM_ (putStrLn . show . LB.length . body) msgs''
--            ;; mapM_ (putStrLn . LB.unpack . headers) msgs''

