module Protocol.Tests where

import Protocol.Message
import Protocol.Message.Encoding
import Protocol.Message.Decoding

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.UUID as UUID hiding (null)
import Data.Time.Clock.POSIX
import Data.Binary.Get
import Data.Word
import Debug.Trace
import Test.QuickCheck
import Data.Functor
import Data.Maybe
import Text.Lipsum.Gen


simpleHeader :: String -> Header
simpleHeader s = 
  Header { msgLength = 1
         , headId = bigUUID
         , version = 1
         , timestamp = posixSecondsToUTCTime 0
         , userId = bigUUID
         , headSegments = [textSegment s]
         }


textSegment :: String -> Segment
textSegment s = Txt $ T.pack s

aByteString = head $ encodeHeader $ simpleHeader "apabepacepa" 

testGetHeader = runGetOrFail getHeader $ BSL.fromStrict aByteString

testGetSegment = runGetOrFail getSegments $ BSL.fromStrict segstr
  where
    segstr = pad $ head $ parsed state
    state =  encodeSegment $ basicState "apabepacepa"

basicState s = EncoderState {
    parsed = [BS.pack []]
    , index = 0
    , chunkRemaining = chunkSize
    , uuid = bigUUID
    , segment = segmentData $ textSegment s
}

bigUUID = fromWords bigNo bigNo bigNo bigNo
  where 
    bigNo = maxBound :: Word32

correctEncodedLength header = all (\l -> l == chunkSize) (traceShow lengths lengths)
  where
    lengths = map BS.length $ encodeHeader header

instance Arbitrary Message where 
  arbitrary = do
    header <- arbitrary
    return $ Message header []

instance Arbitrary Header where
  arbitrary = do
    hID <- arbitrary
    tInt <- arbitrary :: Gen Integer
    let time = posixSecondsToUTCTime $ (fromIntegral . abs) tInt
    uID <- arbitrary
    hSegs <- arbitrary
    return $ Header 1 hID 1 time uID [hSegs] 

instance Arbitrary UUID where
  arbitrary = arbitrary >>= \(a) -> return $ uncurry4 fromWords a
      where
        uncurry4 f (v,x,y,z) = f v x y z

instance Arbitrary Segment where
    arbitrary = Txt <$> (T.pack <$> sentence)


failing = map unBlockify 
    [
        {-Header {msgLength = 1, headId = 0583e874-007c-a197-0768-1d4f035ce4c1, version = 1, timestamp = 1970-01-01 00:00:33 UTC, userId = 0283656b-0196-5d3c-00
92-2f81034025cb, headSegments = [Txt "Tauiy whiniter com unatsienlous crenaling; reiiicenmassy deriti inest sur antleer exowianpum, stotibezes apble a
loily peb, re nur au unraaly byverpuer; egcron, cengatly obecused deiy osouer in?"]}-},
        (Message (Header { msgLength = 1,
          headId = (fromJust . fromString) "00000089-0000-00ad-0000-00cf00000023",
          version = 1,
          timestamp = (posixSecondsToUTCTime . fromIntegral) 6,
          userId = (fromJust . fromString) "0000000c-0000-00c3-0000-002e0000009e",
          headSegments = [Txt $ T.pack "Cersurdleer arnala decraatly deny noisker an; detterest au in pre, exile vercelsel un."]
          })
          []
          )


    ]