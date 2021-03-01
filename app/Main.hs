{-# LANGUAGE ViewPatterns #-}

module Main where
import           Control.Exception                  (Exception, throw, throwIO,
                                                     try)
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Char8              as SB
import           Data.CaseInsensitive               (CI)
import qualified Data.CaseInsensitive               as CI
import           Data.Char                          (isSpace)
import qualified Data.ConfigFile                    as Cfg
import           Data.Either.Utils
import           Data.List
import           Data.List.Split                    (splitOn)
import           Data.Maybe
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Mbox
import           Network.Connection
import           Network.HaskellNet.IMAP.Connection (IMAPConnection)
import           Network.HaskellNet.IMAP.SSL
import           Pipes
import qualified Pipes.Lift                         as PL
import qualified Pipes.Prelude                      as P
import           System.Console.GetOpt
import           System.Environment                 (getArgs, getProgName)
import           System.IO                          (IOMode (..), openFile,
                                                     withFile)
import           Text.Pretty.Simple                 (pPrint)
import           Text.Regex.TDFA

-- TODO: unsued for now
data ImportException =
  ParseError SB.ByteString
  | MissingHeaders SB.ByteString
  | DataLeft SB.ByteString
  deriving Show

instance Exception ImportException


data Config = Config
              { username    :: String
              , password    :: String
              , striplabels :: Regex
              , skiplabels  :: Regex
              }

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

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- state
data ST = ST {
  counter   :: Int
  , folders :: Set String
  }

createFolders :: IMAPConnection -> Options -> Set String -> IO ()
createFolders conn opts fset =
  mapM_ (\f -> do
            if optVerbose opts
              then putStrLn ("\tCreating folder: " ++ (show f))
              else return ()
            if optDryRun opts
              then return ()
              else create conn f
       )
    (Set.toList fset)

processMessage :: Options -> Config -> IMAPConnection -> Message -> Effect (StateT ST IO) ()
processMessage opts cfg conn m =
  let
    testRegexp :: Set String -> Regex -> Bool
    testRegexp s r = not $ Set.null (Set.filter (matchTest r) s)
  in do
    let rawh = headers m
    n <- (lift . gets) counter
    lift $ modify (\s -> s {counter = n+1})
    case splitHeader (unfoldHeader rawh) of
      Nothing ->
        throw $ ParseError rawh
      Just hassoc ->
        let findh name = find ((==) (CI.mk (SB.pack name)) . fst) hassoc >>= (Just . trim . SB.unpack . snd) in
          -- at very least we need this field, to filter out Chat messages
          -- which do not have MessageId
          case findh "X-Gmail-Labels" of
            Just l ->
              let lset = Set.fromList (fmap trim (splitOn "," l)) in
                if testRegexp lset (skiplabels cfg) then
                  if optVerbose opts
                  then liftIO $ putStrLn ("====== Skipping #" <> show n)
                  else return ()
                else do
                  liftIO $ putStrLn ("====== Processing #" <> show n <> ":")
                  let l' = Set.filter (not . matchTest (striplabels cfg)) lset
                  oldfolders <- (lift . gets) folders
                  let newfolders = Set.difference l' oldfolders
                  liftIO $ createFolders conn opts newfolders
                  lift $ modify (\s -> s {folders = Set.union l' (folders s)})
                  liftIO $ putStrLn ("====== Processing #" <> show n <> ":")
            Nothing ->
              throw $ MissingHeaders rawh

-- empty procducer
emptyP :: MonadIO m => Producer SB.ByteString m ()
emptyP = return ()

readConfig :: String -> IO Config
readConfig file =  do
  cfge <- Cfg.readfile Cfg.emptyCP file
  let cfg = forceEither cfge
  let user = forceEither $ Cfg.get cfg "account" "user"
  let pass = forceEither $ Cfg.get cfg "account" "password"
  let skip = (forceEither $ Cfg.get cfg "labels" "skip" ) :: String
  let strip = (forceEither $ Cfg.get cfg "labels" "strip" ) :: String
  return Config { username = user
                , password = pass
                , skiplabels = makeRegex skip
                , striplabels = makeRegex strip
                }

main :: IO ()
main =
    do
      (opts, inputfile) <- getArgs >>= parseOpts
      config  <- readConfig (optConfig opts)
      conn <- connectIMAPSSL "imap.gmail.com"
      login conn (username config) (password config)
      mblist <- list conn
      -- mapM (pPrint . show) (map snd mblist)
      withFile inputfile ReadMode
        (\f ->
           let p =
                 (processMBFile f >->
                  P.drop (optSkip opts)
                  >->
                  (case optLimit opts of
                     Nothing -> cat
                     Just n  -> emptyP <$ P.take n)
                 )
           in
             do
               let st0 = ST { folders = Set.fromList (map snd mblist),
                              counter = (optSkip opts)
                            }
               (restp, st) <- runStateT (runEffect $ for p (processMessage opts config conn)) st0
               logout conn
               rest <- next (PL.evalStateP st restp)
               case rest of
                 Left _      -> return () -- all done
                 Right (s,_) ->
                   -- unprocessed data remains
                   throwIO (DataLeft s)
        )

