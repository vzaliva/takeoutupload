{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

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
import           Data.String.Utils                  (strip)
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
import           System.Time
import           Text.Read                          (readMaybe)

data ImportException =
  ParseError SB.ByteString
  | MissingHeaders SB.ByteString
  | DataLeft SB.ByteString
  | IMAPAppendError
  | MissingDateInFrom SB.ByteString
  | InvalidDateFieldInFrom String SB.ByteString
  deriving Show

instance Exception ImportException

data Config = Config
              { username    :: String
              , password    :: String
              , striplabels :: Regex
              , skiplabels  :: Regex
              , taglabel    :: String
              }

data Options = Options
    { optVerbose :: Bool
    , optDebug :: Bool
    , optDryRun  :: Bool
    , optConfig  :: String
    , optSkip    :: Int
    , optLimit   :: Maybe Int
    } deriving Show

defaultOptions = Options
    { optVerbose     = False
    , optDebug       = False
    , optDryRun      = False
    , optConfig      = "takeoutupload.cfg"
    , optSkip        = 0
    , optLimit       = Nothing
    }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> Right opts { optVerbose = True }))
        "verbose output on stderr"
    , Option ['d']     ["debug"]
        (NoArg (\ opts -> Right opts { optDebug = True }))
        "debug output on stderr"      
    , Option ['n']     ["dry-run"]
        (NoArg (\ opts -> Right opts { optDryRun = True }))
        "dry-run mode - do not modify IMAP account"
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

-- | Quote IMAP folder name
-- If it contains spaces, enclose in double quotation marks
quoteFolder :: String -> String
quoteFolder s = if elem ' ' s then "\"" ++ s ++ "\""  else s

-- state
data ST = ST {
  counter   :: Int
  , folders :: Set String
  }

createFolders :: IMAPConnection -> Options -> Set String -> IO ()
createFolders conn opts fset =
  mapM_ (\f -> do
            when (optVerbose opts)
              $ putStrLn ("\tCreating folder: " ++ (show f))
            unless (optDryRun opts)
              $ create conn (quoteFolder f)
       )
    (Set.toList fset)

readMonth :: String -> Month
readMonth "Jan" = January
readMonth "Feb" = February
readMonth "Mar" = March
readMonth "Apr" = April
readMonth "May" = May
readMonth "Jun" = June
readMonth "Jul" = July
readMonth "Aug" = August
readMonth "Sep" = September
readMonth "Oct" = October
readMonth "Nov" = November
readMonth "Dec" = December

readDay :: String -> Day
readDay "Sun" = Sunday
readDay "Mon" = Monday
readDay "Tue" = Tuesday
readDay "Wed" = Wednesday
readDay "Thu" = Thursday
readDay "Fri" = Friday
readDay "Sat" = Saturday

{- Parse mailbox "From" separator line, extracting date.
   Example:

   From 1461441616689256664@xxx Sun Mar 02 05:48:54 +0000 2014
-}
parseFromDate :: SB.ByteString -> IO CalendarTime
parseFromDate bs =
  let s = SB.unpack bs 
      ns = (/=) ' '
      iss = (==) ' '
      ds = strip $ dropWhile ns $ dropWhile iss $ dropWhile ns s
      f = fmap strip (words ds)
      d = splitOn ":" (f!!3)
      readInt n s = case readMaybe s of
                    Just v -> v
                    Nothing -> throw $ InvalidDateFieldInFrom n bs
  in
    if length f == 6
    then
      if f!!4 == "+0000"
      then
        return CalendarTime
      { ctYear    = readInt "year" (f!!5)
      , ctMonth   = readMonth (f!!1)
      , ctDay     = readInt "day" (f!!2)
      , ctHour    = readInt "hour" (d!!0)
      , ctMin     = readInt "min" (d!!1)
      , ctSec     = readInt "sec" (d!!2)
      , ctPicosec = 0
      , ctWDay    = readDay (f!!0)
      , ctYDay    = 0 -- Boldly assume this field is unused
      , ctTZName  = f!!4
      , ctTZ      = 0
      , ctIsDST   = False
      }
      else throw $ MissingDateInFrom bs
    else throw $ MissingDateInFrom bs

uploadMessage :: Options -> IMAPConnection -> String -> SB.ByteString -> String -> Set String -> SB.ByteString -> IO ()
uploadMessage opts conn msgid msgdates tag folders msg =
  let msglf = SB.filter ((/=) '\r') msg
      folders' = Set.delete tag folders
  in do
    -- putStrLn (SB.unpack msg)
    msgdate <- parseFromDate msgdates
    when (optVerbose opts) $ putStrLn ("\tMsgID: " <> show msgid)
    appendFull conn (strip tag) msglf (Just [Seen]) (Just msgdate)
    -- Add labels
    unless (Set.null folders') 
      do
        uids <- search conn [HEADERs "Message-ID" msgid]
        case uids of
          []    -> throw IMAPAppendError
          uids -> do
            let flist = Set.toList folders'
            when (optVerbose opts) $ putStrLn ("\t+Labels: " <> show flist)
            mapM_ (\uid -> store conn uid (PlusGmailLabels flist)) uids

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
        let findh name = find ((==) (CI.mk (SB.pack name)) . fst) hassoc >>= (return . strip . SB.unpack . snd) in
          -- at very least we need this field, to filter out Chat messages
          -- which do not have MessageId
          case findh "X-Gmail-Labels" of
            Just l ->
              let lset = Set.fromList (fmap strip (splitOn "," l)) in
                if testRegexp lset (skiplabels cfg) then
                  liftIO $ putStrLn ("====== Skipping #" <> show n)
                else do
                  liftIO $ putStrLn ("====== Processing #" <> show n <> ":")
                  let l' = Set.filter (not . matchTest (striplabels cfg)) lset
                  oldfolders <- (lift . gets) folders
                  let newfolders = Set.difference l' oldfolders
                  liftIO $ createFolders conn opts newfolders
                  lift $ modify (\s -> s {folders = Set.union l' (folders s)})
                  let lf = SB.pack "\n"
                  let rawmsg = (headers m) `SB.append` lf `SB.append` (body m)
                  case findh "Message-ID" of
                    Just mid ->
                      unless (optDryRun opts) $
                      liftIO $ uploadMessage opts conn mid (fromLine m) (taglabel cfg) l' rawmsg
                    _ -> throw $ MissingHeaders rawh
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
  let tag = forceEither $ Cfg.get cfg "labels" "tag"
  let skip = (forceEither $ Cfg.get cfg "labels" "skip" ) :: String
  let strip = (forceEither $ Cfg.get cfg "labels" "strip" ) :: String
  return Config { username = user
                , password = pass
                , skiplabels = makeRegex skip
                , striplabels = makeRegex strip
                , taglabel = tag
                }

main :: IO ()
main =
    do
      (opts, inputfile) <- getArgs >>= parseOpts
      config  <- readConfig (optConfig opts)
      conn <-
        if optDebug opts
        then connectIMAPSSLWithSettings "imap.gmail.com"
             (defaultSettingsIMAPSSL {sslLogToConsole = True})
        else connectIMAPSSL "imap.gmail.com"
      login conn (username config) (password config)
      mblist <- list conn
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
               let server_folders = Set.fromList (map snd mblist)
               -- Make sure "tag" folder exists
               let tagset = Set.singleton (taglabel config)
               unless (Set.member (taglabel config) server_folders)
                 $ createFolders conn opts tagset
               select conn (taglabel config)
               let st0 = ST { folders = Set.union server_folders tagset,
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

