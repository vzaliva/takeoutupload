{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as SB
import           Data.CaseInsensitive  (CI)
import qualified Data.CaseInsensitive  as CI
import           Data.Char             (isSpace)
import           Data.List
import           Data.List.Split
import           Mbox
import           Pipes
import qualified Pipes.Prelude         as P
import           System.Console.GetOpt
import           System.Environment    (getArgs, getProgName)
import           System.IO             (IOMode (..), openFile, withFile)

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

-- | Unfold header lines per RFC 2822:
-- "Unfolding is accomplished by simply removing any CRLF
--  that is immediately followed by WSP."
unfoldHeader :: SB.ByteString -> SB.ByteString
unfoldHeader (SB.uncons -> Nothing) = SB.empty
unfoldHeader (SB.uncons -> Just ('\r', SB.uncons -> Just ('\n', SB.uncons -> Just(' ',xs)))) = SB.cons ' ' (unfoldHeader xs)
unfoldHeader (SB.uncons -> Just ('\r', SB.uncons -> Just ('\n', SB.uncons -> Just('\t',xs)))) = SB.cons '\t' (unfoldHeader xs)
unfoldHeader (SB.uncons -> Just ('\r', xs)) = unfoldHeader xs
unfoldHeader (SB.uncons -> Just (x, xs)) = SB.cons x (unfoldHeader xs)

-- | Split header line into header name (case-insensitive) and value
splitHeader :: SB.ByteString -> Maybe [(CI SB.ByteString, SB.ByteString)]
splitHeader =
  let split s = do
        i <- SB.elemIndex ':' s
        let (n,v) = SB.splitAt i s
        return (CI.mk n, SB.tail v) in
    sequence . map split . SB.lines

-- | Message headers we are interested in
data Headers =  Headers {
      msgid  :: String,
      labels :: [String]
      } deriving Show

-- | Given message header block, try to extract relevant headers
extractHeaders :: SB.ByteString -> Maybe Headers
extractHeaders rawh = do
  hassoc <- splitHeader (unfoldHeader rawh)
  let trim = dropWhileEnd isSpace . dropWhile isSpace
  let findh name = find ((==) (CI.mk (SB.pack name)) . fst) hassoc >>= (return . trim . SB.unpack . snd) in
    do
    msgid <- findh "message-id"
    labels <- findh "X-Gmail-Labels"
    return Headers { msgid = msgid, labels = fmap trim (splitOn "," labels)}

processMessage :: Message -> Effect IO ()
processMessage m = do
    (lift . putStrLn) "====== Processing:"
    (lift . putStrLn) "--- From:"
    (lift . putStrLn . SB.unpack . fromLine) m
    (lift . putStrLn) "--- Relevant Headers:"
    (lift . putStrLn . show . extractHeaders . headers) m
    (lift . putStrLn) "--- Body length:"
    (lift . putStrLn . show . SB.length . body) m
    (lift . putStrLn) "--- All headers:"
    (lift . putStrLn . SB.unpack . headers) m

main :: IO ()
main =
    do
      (opts, inputfile) <- getArgs >>= parseOpts ;
      withFile inputfile ReadMode
        (\f ->
           let p =
                 (processMBFile f >->
                  P.drop (optSkip opts)
-- TODO: implement LIMIT
--                  >->
--                  (case optLimit opts of
--                     Nothing -> cat
--                     Just n  -> P.take n)
                 )
           in
             do
               runEffect $ for p processMessage
               return ()
        )

