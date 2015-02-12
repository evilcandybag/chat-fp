module Protocol where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Text
import Data.Bits
import Data.UUID
import Data.Binary.Get
import Data.Maybe

data Message = Message Header [Block]

data Header = Header { headIndex    :: Index
                     , headId       :: Ident
                     , version      :: Version
                     , timestamp    :: Timestamp
                     , userid       :: UserID
                     , headSegments :: [Segment]
                     }

data Block = Block { blockIndex    :: Index
                   , blockId       :: Ident
                   , blockSegments :: [Segment]
               }
type Index = Integer

type Ident = UUID

type Version = Integer

type Timestamp = UTCTime

type UserID = BS.ByteString



data Segment = Txt Text
             | Img BS.ByteString
             | CustomTxt Text
             | CustomBin BS.ByteString


fromBytes :: BS.ByteString -> Either Header Block
fromBytes bs = if testBit (BS.head bs) 0
  then Left $ parseHeader bs
  else Right $ parseBlock bs

parseBlock :: BS.ByteString -> Block
parseBlock bs = runGet getBlock $ BSL.fromStrict bs
  where 
    getBlock = do
        ix <- fmap (flip setBit $ 0) getWord16host
        uuid <- fmap fromByteString (getByteString 16)
        segments <- parseSegments getRemainingLazyByteString
        return Block {
            blockIndex = fromIntegral ix,
            blockId = fromJust uuid,
            blockSegments = segments
            }



parseHeader :: BS.ByteString -> Header
parseHeader bs = undefined

parseSegments :: BSL.ByteString -> [Segment]
parseSegments = undefined