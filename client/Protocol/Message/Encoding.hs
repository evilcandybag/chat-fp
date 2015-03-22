module Protocol.Message.Encoding where


import Protocol.Message
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Builder
import Data.Text.Encoding
import Data.Word
import Data.UUID
import Data.Monoid
import Data.Time.Clock.POSIX
import Data.Bits
import Debug.Trace

type SegmentData = (Word8, Int, ByteString)

data EncoderState = 
  EncoderState { parsed :: [ByteString]
               , index :: Word16
               , chunkRemaining :: Int
               , segment :: SegmentData
               , uuid :: UUID
               }
               deriving Show

-- | Encode a header into a list of ByteStrings, each chunkSize bits long. 
encodeHeader :: Header -> [ByteString]
encodeHeader header = if numBlocks < maxIndex
    then head':blocks
    else error $ "Attempting to encode too many blocks: " ++ (show numBlocks) ++
                 " max encodable: " ++ (show maxIndex)
  where
    initialState = EncoderState 
        { parsed = [bsHeader]
        , index = 0
        , chunkRemaining = chunkSize - BS.length bsHeader
        , uuid = hID
        , segment = seg 
        }
    bsHeader = builderToBS headerBuilder
    headerBuilder = word16LE 0 <> uuidToBS hID <> word8 (version header) <> 
        word32LE time <> uuidToBS uID 
    hID = headId header 
    uID = userId header
    uuidToBS = (lazyByteString . toByteString)
    builderToBS = (BSL.toStrict . toLazyByteString)
    time = floor . utcTimeToPOSIXSeconds $ timestamp header
    (seg:segs) = map segmentData $ headSegments header
    finalState = encodeSegments initialState segs
    (head:blocks) = reverse $ parsed finalState
    numBlocks = length blocks
    head' = builderToBS $ bsLength <> byteString (BS.drop 2 head)
    hLength = index finalState
    bsLength = word16LE (hLength `setBit` 0)

encodeSegments :: EncoderState -> [SegmentData] -> EncoderState
encodeSegments state [] = state' { parsed = (pad bs : bss) } 
  where 
    state' = encodeSegment state 
    (bs:bss) = parsed state'
encodeSegments state (seg:segs) = encodeSegments state' {segment = seg} segs
  where
    state' = encodeSegment state

encodeSegment :: EncoderState -> 
                    EncoderState
encodeSegment  state
  | left < min = encodeBlock state { parsed = (pad bs:bss)
                                   , segment = s
                                   } 
  | otherwise = if bsLength > left && BS.length dat' > 0
    then encodeBlock state { parsed = bss' 
                           , segment = s'
                           }
    else state { parsed = (bs':bss)
               , segment = (typ, min, BS.empty)
               , chunkRemaining = left'
               }
  where
    bs' = BS.append bs (typ `BS.cons` (fromIntegral len-2) `BS.cons` dat)
    bsLength = BS.length bs'
    len = Prelude.min (2 + BS.length dat) left
    left' = left - bsLength
    bss' = (pad bs'':bss)
    s' = (typ,min, dat')
    (bs'',dat') = BS.splitAt chunkSize bs'
    (bs:bss) = parsed state
    s@(typ,min,dat) = segment state
    left = chunkRemaining state

pad :: ByteString -> ByteString
pad bs = BS.append bs (BS.pack $ take n padding)
  where
    padding = [0..] :: [Word8]
    n = chunkSize - (BS.length bs)


encodeBlock :: EncoderState -> EncoderState 
encodeBlock state = encodeSegment state {parsed = (blockPrefix:parsed state)}
  where
    left = chunkSize - BS.length blockPrefix 
    blockPrefix = BSL.toStrict $ toLazyByteString $ word16LE index' <> 
        (lazyByteString $ toByteString $ uuid state)
    index' = succ $ index state


-- | Get the data in a Segment as a ByteString
segmentData :: Segment -> SegmentData
segmentData s = case s of 
  Txt text       -> (1, 3, encodeUtf8 text)
  Img bs         -> (2, 18, BSL.toStrict $ toByteString bs)
  CustomTxt text -> (3, 3, encodeUtf8 text)
  CustomBin bs   -> (4, 3, bs)
  END            -> (0, 1, BS.empty)


