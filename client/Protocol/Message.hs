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
import Debug.Trace

data Message = Message Header [Block]
  deriving Show

data Header = Header { msgLength    :: !Index
                     , headId       :: !Ident
                     , version      :: !Version
                     , timestamp    :: !Timestamp
                     , userId       :: !UserID
                     , headSegments :: ![Segment]
                     }
  deriving Show

data Block = Block { blockIndex    :: !Index
                   , blockId       :: !Ident
                   , blockSegments :: ![Segment]
                   }
  deriving Show

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
  deriving Show

instance Eq Block where 
  (==) a b = sameId a b && (blockIndex a) == (blockIndex b)

instance Ord Block where 
  compare a b = compare (blockIndex a) (blockIndex b)

-- | Simple equality by unique identifier. Sufficient for most cases. 
instance Eq Header where
  (==) a b = headId a == headId b 

-- | More thorough equality check that goes through all properties. 
headerEquals :: Header -> Header -> Bool
headerEquals a b = headId a == headId b
                && msgLength a == msgLength b
                && timestamp a == timestamp b
                && userId a == userId b
                && headSegments a == headSegments b


instance Eq Segment where
  (==) (Txt ta) (Txt tb) = ta == tb
  (==) (Img ida) (Img idb) = ida == idb
  (==) (CustomTxt cta) (CustomTxt ctb) = cta == ctb
  (==) (CustomBin cba) (CustomBin cbb) = cba == cbb
  (==) END END = True
  (==) _ _ = False 

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
unBlockify (Message header []) = header { headSegments = hSegs }
  where 
    hSegs = mergeSegments $ headSegments header
unBlockify msg@(Message h blocks) 
  | complete msg = h { headSegments = mergeSegments $ concat (hSegs:bSegs) }
    where
      hSegs = headSegments h
      bSegs = map blockSegments blocks

mergeSegments :: [Segment] -> [Segment]
mergeSegments [] = []
mergeSegments ((Txt t1):(Txt t2):ss) = mergeSegments ((Txt $ T.append t1 t2):ss)
mergeSegments (s:ss) = (s:mergeSegments ss)

chunkSize :: Int
chunkSize = 128

maxIndex :: Int
maxIndex = fromIntegral $ (maxBound :: Word16) `clearBit` 0 