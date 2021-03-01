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

{-# LANGUAGE RankNTypes #-}

module Mbox
    ( processMBFile,
      Message(..),
    ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as SB
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Lens.Family           (view)
import           Pipes
import qualified Pipes.ByteString      as PB
import           Pipes.Group           as PG
import           Pipes.Parse           as PP
import qualified Pipes.Prelude         as P
import           Prelude               hiding (mconcat)
import           System.IO
import           Text.Regex.TDFA


-- | Message consists of "From" line separator, headears and body
data Message = Message {
      fromLine :: SB.ByteString,
      headers  :: SB.ByteString,
      body     :: SB.ByteString
    } deriving (Read, Show)


-- Predicate checking if given String prefix (1st argument) matches
-- given ByteString
lbsw :: String -> SB.ByteString -> Bool
lbsw p s = SB.isPrefixOf (SB.pack p) s

unmungeFrom :: SB.ByteString -> SB.ByteString
unmungeFrom x = if x =~ ">+From " then SB.tail x else x

{-| Draws elements from producer until it is empty or given predicate
    no longer holds for the next drawn element. The element for
    which it did not hold remains in the stream.
-}
drawWhile :: Monad m => (a -> Bool) -> Parser a m [a]
drawWhile p = go id
  where
    go diffAs = do
        x <- draw
        case x of
            Nothing -> return (diffAs [])
            Just a  ->
              if p a then
                go (diffAs . (a:))
              else do
                unDraw a
                return (diffAs [])

parseMessage :: MonadIO m => Parser SB.ByteString m (Maybe Message)
parseMessage = do
  from <- draw
  case from of
    Nothing   -> return Nothing
    Just from ->
      -- Must start with "From"
      if lbsw "From " from then
        do
            -- headers until first empty line
            hlines <- drawWhile ((/=) (SB.pack "\r"))
            -- skip empty line
            skip
            -- body until next "From" or end of stream
            mblines <- drawWhile (not . lbsw "From ")
            case (hlines, mblines) of
              (_:_, _:_) ->
                let blines = map unmungeFrom mblines in
                  return (Just (Message { fromLine = from,
                                          headers  = SB.unlines hlines,
                                          body     = SB.unlines blines}))
              (_,_) -> return Nothing
        else
          return Nothing

-- https://stackoverflow.com/questions/37632027/haskell-pipes-how-to-repeatedly-perform-a-takewhile-operation-on-a-bytestring
mconcats :: (Monad m, Monoid b) => PB.FreeT (Producer b m) m r -> Producer b m r
mconcats = PG.folds (<>) mempty id

splitLines :: MonadIO m => Producer SB.ByteString m () -> Producer SB.ByteString m ()
splitLines = mconcats . view PB.lines

-- | Reads mbox file as list of messages

processMBFile :: MonadIO m => Handle -> Producer Message m (Producer SB.ByteString m ())
processMBFile hfile =
  let bytes = PB.fromHandle hfile
      lines = splitLines bytes
  in
    PP.parsed_ parseMessage lines





