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
      Message(..)
    ) where

import           Control.Foldl              (mconcat, purely)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.List
import           Data.Maybe
import           Lens.Family                (view)
import           Pipes
import qualified Pipes.ByteString           as PB
import           Pipes.Group                as PG
import           Pipes.Parse
import qualified Pipes.Prelude              as P
import           Prelude                    hiding (mconcat)
import           System.IO
import           Text.Regex.TDFA

-- | Message consists of "From" line separator, headears and body
data Message = Message {
      fromLine :: LB.ByteString,
      headers  :: LB.ByteString,
      body     :: LB.ByteString
    } deriving (Read, Show)

parseMessages :: Pipe ByteString Message IO ()
parseMessages = return ()

-- https://stackoverflow.com/questions/37632027/haskell-pipes-how-to-repeatedly-perform-a-takewhile-operation-on-a-bytestring
mconcats :: (Monad m, Monoid b) => PB.FreeT (Producer b m) m r -> Producer b m r
mconcats = PG.folds (<>) mempty id

splitLines :: Producer ByteString IO () -> Producer ByteString IO ()
splitLines = mconcats . view PB.lines

-- | Reads mbox file as list of messages
processMBFile :: Handle -> Producer Message IO ()
processMBFile hfile =
  splitLines (PB.fromHandle hfile) >-> parseMessages

readMessage :: [LB.ByteString] -> Maybe (Message,[LB.ByteString])
readMessage [] = Nothing
readMessage (from:xs) =
  if lbsw "From " from then
    let
      (mblines, rest) = break (lbsw "From ") xs
      (hlines, blines) = break (\x -> LB.length x == 1 && LB.head x == '\r') mblines
      blines' = map unmungeFrom blines in
      return (Message { fromLine = from,
                        headers  = LB.unlines hlines,
                        body     = LB.unlines blines'}, rest)
  else
    Nothing
  where
    lbsw p s = LB.isPrefixOf (LB.pack p) s
    unmungeFrom x = let fromreg = ">+From " in
      if x =~ fromreg then LB.tail x else x


processMB :: [LB.ByteString] -> [Message]
processMB ls = case readMessage ls of
                 Nothing      -> []
                 Just (m,ls') -> m:(processMB ls')


