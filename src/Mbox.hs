{-|
Module      : Mbox
Description : MBox file parser
Copyright   : (c) Vadim Zaliva, 2020
License     : GPL-3
Maintainer  : lord@crocodile.org
Stability   : experimental

MBox file parser. It does not parse individual message headers but
rather splits the mailbox into "messages" consisting of headers and
body as ByteStrings. For body it performs ">From" unmungling.

The implementation supposed to be "lazy" and if properly used will
allow to process a mailbox witout loading it into memory.
-}
module Mbox
    ( processMBFile,
      Message(..),
      ParseException(..)
    ) where

import           Control.Exception     (Exception, throwIO, try)
import           Control.Foldl         (mconcat, purely)
import           Control.Monad         (unless)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as SB
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Lens.Family           (view)
import           Pipes
import qualified Pipes.ByteString      as PB
import           Pipes.Group           as PG
import           Pipes.Parse
import qualified Pipes.Prelude         as P
import           Prelude               hiding (mconcat)
import           System.IO
import           Text.Regex.TDFA

data ParseException = NoFrom SB.ByteString
    deriving Show
instance Exception ParseException

-- | Message consists of "From" line separator, headears and body
data Message = Message {
      fromLine :: SB.ByteString,
      headers  :: SB.ByteString,
      body     :: SB.ByteString
    } deriving (Read, Show)

-- Predicate checking if given String prefix (1st argument) matches
-- given ByteString
lbsw :: String -> ByteString -> Bool
lbsw p s = SB.isPrefixOf (SB.pack p) s

parseMessage :: ByteString -> Pipe ByteString Message IO ()
parseMessage from =
    let
      (hproducer, bproducer) = PB.breakOn (SB.pack "\r")
      bproducer1 = P.map unmungeFrom bproducer
      blines = produceAll bproducer
      hlines = produceAll hproducer
    in
      yeld (Message { fromLine = from,
                      headers  = SB.unlines hlines,
                      body     = SB.unlines blines'})
  where
    unmungeFrom x = if x =~ ">+From " then SB.tail x else x

parseMessages :: Pipe ByteString Message IO ()
parseMessages = do
  from <- await
  if lbsw "From " from then
    P.takeWhile (not . lbsw "From ") >-> parseMessage from
  else
     lift $ throwIO (NoFrom from)

-- https://stackoverflow.com/questions/37632027/haskell-pipes-how-to-repeatedly-perform-a-takewhile-operation-on-a-bytestring
mconcats :: (Monad m, Monoid b) => PB.FreeT (Producer b m) m r -> Producer b m r
mconcats = PG.folds (<>) mempty id

splitLines :: Producer ByteString IO () -> Producer ByteString IO ()
splitLines = mconcats . view PB.lines

-- | Reads mbox file as list of messages
processMBFile :: Handle -> Producer Message IO ()
processMBFile hfile =
  splitLines (PB.fromHandle hfile) >-> parseMessages






readMessage :: [SB.ByteString] -> Maybe (Message,[SB.ByteString])
readMessage [] = Nothing
readMessage (from:xs) =
  if lbsw "From " from then
    let
      (mblines, rest) = break (lbsw "From ") xs
      (hlines, blines) = break (\x -> SB.length x == 1 && SB.head x == '\r') mblines
      blines' = map unmungeFrom blines in
      return (Message { fromLine = from,
                        headers  = SB.unlines hlines,
                        body     = SB.unlines blines'}, rest)
  else
    Nothing
  where
    lbsw p s = SB.isPrefixOf (SB.pack p) s
    unmungeFrom x = let fromreg = ">+From " in
      if x =~ fromreg then SB.tail x else x


processMB :: [SB.ByteString] -> [Message]
processMB ls = case readMessage ls of
                 Nothing      -> []
                 Just (m,ls') -> m:(processMB ls')


