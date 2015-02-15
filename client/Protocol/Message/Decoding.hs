module Protocol.Message.Decoding where

import Protocol.Message

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Text (Text)
import Data.Text.Encoding
import Data.Bits
import Data.UUID
import Data.Binary.Get
import Data.Maybe
import qualified Data.Map as Map

type Chunk = Either Header Block

type SortedBlocks = Map.Map UUID [Block]


-- | Parse a list of ByteString into a list of Messages, and a tuple containing whatever 
--   Headers and Blocks left over. 
--   Assumes that each ByteString is 128 bytes in length.
decodeMessages :: [BS.ByteString] -> ([Message], ([Header],SortedBlocks))
decodeMessages bss = mkMessages headers bMap
  where
    (headers, bMap) = sortChunks $ map fromBytes bss

mkMessages :: [Header] ->  SortedBlocks -> ([Message], ([Header],SortedBlocks))
mkMessages (h:hs) bMap = (msgs, restBlocks')
  where 
    (msg,restBlocks)   = mkMessage h bMap
    (msgs,restBlocks') = mkMessages hs restBlocks

-- | Construct a message from a header and a set of blocks. 
mkMessage :: Header -> SortedBlocks -> (Message, SortedBlocks)
mkMessage header bMap = let 
  hID = headId header
  blocks = Map.lookup hID bMap
    in case blocks of 
      Just blocks' -> (Message header blocks', Map.delete hID bMap)
      Nothing      -> (Message header [], bMap)
      

sortChunks :: [Chunk] ->  ([Header], SortedBlocks)
sortChunks = sortChunks' ([],Map.empty)
  where 
    sortChunks' chunks [] = chunks
    sortChunks' (hs, bMap) (chunk:chunks) = case chunk of 
      Left header -> sortChunks' ((header:hs),bMap) chunks
      Right block   -> sortChunks' (hs, Map.insertWith (++) (blockId block) [block] bMap) chunks

fromBytes :: BS.ByteString -> Chunk
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