module Protocol.Message where

import Data.ByteString (ByteString)
import Data.Word
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Bits
import Data.UUID
import Data.Binary.Get
import Data.Maybe
import qualified Data.Map as Map
import Data.List

data Message = Message Header [Block]

data Header = Header { msgLength    :: !Index
                     , headId       :: !Ident
                     , version      :: !Version
                     , timestamp    :: !Timestamp
                     , userId       :: !UserID
                     , headSegments :: ![Segment]
                     }

data Block = Block { blockIndex    :: !Index
                   , blockId       :: !Ident
                   , blockSegments :: ![Segment]
                   }
type Index = Integer

type Ident = UUID

type Version = Word8

type Timestamp = UTCTime

type UserID = UUID

data Segment = Txt Text
             | Img UUID
             | CustomTxt Text
             | CustomBin ByteString
             | END

instance Eq Block where 
  (==) a b = sameId a b && (blockIndex a) == (blockIndex b)

instance Ord Block where 
  compare a b = compare (blockIndex a) (blockIndex b)

instance Eq Header where
  (==) a b = (headId a) == (headId b)

instance Ord Header where 
  compare a b = compare (timestamp a) (timestamp b)

-- | Checks if a Message has all the Blocks declared in its header. 
complete :: Message -> Bool
complete (Message h bs) = (msgLength h) == (fromIntegral $ length bs')
  where 
    bs' = (head . group . sort) bs 

sameId :: Block -> Block -> Bool
sameId a b = blockId a == blockId b

-- | Merge all blocks into the Header
unBlockify :: Message -> Header
unBlockify (Message header []) = header
unBlockify msg@(Message h blocks) 
  | complete msg = h { headSegments = mergeSegments $ concat (hSegs:bSegs) }
    where
      hSegs = headSegments h
      bSegs = map blockSegments blocks

mergeSegments :: [Segment] -> [Segment]
mergeSegments [] = []
mergeSegments ((Txt t1):(Txt t2):ss) = (Txt $ T.append t1 t2):mergeSegments ss
mergeSegments (s:ss) = (s:mergeSegments ss)

chunkSize :: Int
chunkSize = 128

maxIndex :: Int
maxIndex = fromIntegral $ (maxBound :: Word16) `clearBit` 0 