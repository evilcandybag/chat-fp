module Encoding where


import Protocol.Message
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.Text.Encoding

-- | Encode a header into a list of ByteStrings, each 128 bits long. 
encodeHeader :: Header -> [ByteString]


encodeSegment :: ByteString -> [ByteString] -> Segment -> Int -> (Builder, [ByteString], Int) 
encodeSegment bs bss seg left =  

encodeSegment' :: ByteString -> (Word8,ByteString) -> Int -> (ByteString, ByteString, Int)
encodeSegment' bs (typ,dat) left = if dataLength <= left 
    then (append bs (typ `snoc` dat), BS.empty, left - dataLength)
    else 
  where
    dataLength = 1 + BS.length dat

-- | Get the data in a Segment as a ByteString
segmentData :: Segment -> (Word8, ByteString)
segm s = case s of 
  Txt text       -> (1, encodeUtf8 text)
  Img bs         -> (2, bs)
  CustomTxt text -> (3, encodeUtf8 text)
  CustomBin bs   -> (4, bs)
  END            -> (0, BS.empty)

