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

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.List
import           Data.Maybe
import           System.IO
import           Text.Regex.TDFA

-- | Message consists of "From" line separator, headears and body
data Message = Message {
      fromLine :: LB.ByteString,
      headers  :: LB.ByteString,
      body     :: LB.ByteString
    } deriving (Read, Show)

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

-- | Reads mbox file as list of messages
processMBFile :: FilePath -> IO [Message]
processMBFile file = do
    bs <- LB.readFile file
    return (processMB (LB.lines bs))

