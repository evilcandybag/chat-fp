module Protocol where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Text
import Data.Text.Encoding
import Data.Bits
import Data.UUID
import Data.Binary.Get
import Data.Maybe
import Data.Map

data Message = Message Header [Block]

data Header = Header { msgLength    :: !Index
                     , headId       :: !Ident
                     , version      :: !Version
                     , timestamp    :: !Timestamp
                     , userid       :: !UserID
                     , headSegments :: ![Segment]
                     }

data Block = Block { blockIndex    :: !Index
                   , blockId       :: !Ident
                   , blockSegments :: ![Segment]
               }
type Index = Integer

type Ident = UUID

type Version = Integer

type Timestamp = UTCTime

type UserID = UUID



data Segment = Txt Text
             | Img BS.ByteString
             | CustomTxt Text
             | CustomBin BS.ByteString
             | END


parseMessages :: [BS.ByteString] -> ([Message], ([Header],[Block])
parseMessages bss = 
  where
    chunks = map fromBytes bss

sortChunks :: ([Header], Map UUID [Block]) -> [Either Header Block] ->  ([Header], Map UUID [Block])
sortChunks chunks [] = chunks
sortChunks (hs, bMap) (chunk:chunks) = case chunk of 
  Right header -> sortChunks ((header:hs),bMap) chunks
  Left block   -> sortChunks (hs, insert (blockId block) block bMap) chunks

fromBytes :: BS.ByteString -> Either Header Block
fromBytes bs = if testBit (BS.head bs) 0
  then Left $ parseHeader bs
  else Right $ parseBlock bs

parseBlock :: BS.ByteString -> Block
parseBlock bs = runGet getBlock $ BSL.fromStrict bs

getBlock :: Get Block
getBlock = do
  ix <- getIx
  uuid <- getUUID
  segments <- getSegments
  return Block {
            blockIndex = ix,
            blockId = nil,  
            blockSegments = segments
            }

getIx :: Get Integer
getIx = fmap (fromIntegral . (flip clearBit 0)) getWord16le

getUUID :: Get UUID
getUUID = do 
  bs <- getByteString 16
  return $ case (fromByteString . BSL.fromStrict) bs of 
          Just ident -> ident
          Nothing -> error $ 
            "Invalid length of ByteString for UUID. Expected 16, got: " ++ 
              (show $ BS.length bs)

parseHeader :: BS.ByteString -> Header
parseHeader bs = undefined
getHeader :: Get Header 
getHeader = do
  ix <- getIx
  msgId <- getUUID
  ver <- fmap fromIntegral getWord8
  time <- fmap (posixSecondsToUTCTime . fromIntegral) getWord32le 
  usrId <- getUUID
  segments <- getSegments
  return Header {
          msgLength = ix,
          headId = msgId,
          version = ver,
          timestamp = time,
          userid = usrId,
          headSegments = segments
          }

getSegments :: Get [Segment]
getSegments = do
  empty <- isEmpty
  if empty
    then return []
    else do seg <- getSegment
            segs <- getSegments
            return $ case seg of 
              END -> [seg]
              _ -> (seg:segs)

getSegment :: Get Segment 
getSegment = do
  typ <- getWord8
  len <- getWord8
  bytes <- getByteString (fromIntegral len)
  return $ case typ of 
    0 -> END
    1 -> Txt $ decodeUtf8 bytes
    2 -> Img bytes
    3 -> CustomTxt $ decodeUtf8 bytes
    4 -> CustomBin bytes
    x -> error $ "Segment with unknown type byte: " ++ show x