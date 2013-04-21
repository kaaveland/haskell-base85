module Main(main) where

import Codec.Base85.Internal hiding(fromString)

import Control.Exception
import Data.Char
import Data.Word

import qualified Data.ByteString.Lazy as BS

import Test.Framework(Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.Framework.Providers.HUnit(testCase)

import Test.HUnit hiding(Test)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ rfc1924Tests
        , ascii85Tests
        , word32Tests
        , encodingTests
        , wordConversions
        , encoderDecoderTests]

toChar :: Word8 -> Char
toChar = chr . fromIntegral

fromChar :: Integral a => Char -> a
fromChar = fromIntegral . ord

fromString :: String -> BS.ByteString
fromString = BS.pack . map fromChar

rfc1924String :: String
rfc1924String = map toChar rfc1924Alphabet

rfc1924Tests :: Test
rfc1924Tests = testGroup "rfc1924" 
  [ testCase "length(TT)" $ assertEqual "length" 85 $ length rfc1924Alphabet
  , testCase "printable" $ assertBool "alphabet is not printable"
    (all isPrint rfc1924String)
  , testCase "ascii" $ assertBool "alphabet contains non-ascii chars"
    (all isAscii rfc1924String)]

ascii85String :: String
ascii85String = map toChar ascii85Alphabet

ascii85Tests :: Test
ascii85Tests = testGroup "ascii85"
  [ testCase "length(TT)" $ assertEqual "length" 85 $ length ascii85Alphabet
  , testCase "printable" $ assertBool "alphabet is not printable"
    (all isPrint ascii85String)
  , testCase "ascii" $ assertBool "alphabet contains non-ascii chars"
    (all isAscii ascii85String) ]

chunk :: BS.ByteString
chunk = BS.pack [0x12, 0x34, 0x56, 0x78, 0x9A]

word32Tests :: Test
word32Tests = testGroup "word32 encoding"
  [ testProperty "Word32 encodes to 5 bytes with rfc1924"
    (\w32 -> BS.length (encodeWord32 (encoding rfc1924) w32) == 5)
  , testProperty "Word32 encodes to 5 bytes with ascii85"
    (\w32 -> BS.length (encodeWord32 (encoding ascii85) w32) == 5)
  , testCase "w32 encoder" $ assertEqual "encodeW32" 
    (fromString "&i<X6") (encodeWord32 (encoding ascii85) 305419896)
  , testCase "w32 encoder" $ assertEqual "encodeW32"
    (fromString "RK*<f") (encodeWord32 (encoding ascii85) 2583691264)
  , testCase "chunkToW32" $ assertEqual "chunkToW32BE" 305419896 $
    chunkToWord32BE chunk
  , testCase "w32 encode chunk" $ assertEqual "encode chunk"
    (fromString "&i<X6") (encodeChunkBE ascii85 chunk)]

encodingTests :: Test
encodingTests = testGroup "encoding bytestrings"
  [ testCase "bs encoder" $ assertEqual "encode bs"
    (fromString "&i<X6RK") (encode (encodeChunkBE ascii85) chunk)
  , testProperty "should not fail for any BS"
    (\bs -> (evaluate $ encodeBE rfc1924 (fromString bs)) `seq` True)
  , testProperty "should not fail for any BS"
    (\bs -> (evaluate $ encodeBE ascii85 (fromString bs)) `seq` True)
  , testProperty "should not fail for any BS (LE)"
    (\bs -> (evaluate $ encodeLE rfc1924 (fromString bs)) `seq` True)
  , testProperty "should not fail for any BS (LE)"
    (\bs -> (evaluate $ encodeLE ascii85 (fromString bs)) `seq` True)
  ]

wordConversions :: Test
wordConversions = testGroup "Conversions from Word32 to Word8 and back"
  [ testProperty "should be reversible (BE)"
    (\w32 -> w32 == (toWord32BE $ unWord32BE w32))
  , testProperty "should be reversible (LE)"
    (\w32 -> w32 == (toWord32LE $ unWord32LE w32))]

mkEncodeDecode :: Base85 -> BS.ByteString -> Bool
mkEncodeDecode alpha bs = bs == (decodeBE alpha $ encodeBE alpha bs)

mkEncodeDecodeLE :: Base85 -> BS.ByteString -> Bool
mkEncodeDecodeLE alpha bs = bs == (decodeLE alpha $ encodeLE alpha bs)

encoderDecoderTests :: Test
encoderDecoderTests = testGroup "Conversions to base85 and back"
  [ testProperty "should be reversible (BE, rfc1924)" $
    \bl -> mkEncodeDecode rfc1924 $ BS.pack bl
  , testProperty "should be reversible (BE, ascii85)" $
    \bl -> mkEncodeDecode ascii85 $ BS.pack bl
  -- , testProperty "should be reversible for (LE, rfc1924)" $
  --   \bl -> mkEncodeDecodeLE rfc1924 $ BS.pack bl
  -- , testProperty "should be reversible for (LE, ascii85)" $
  --   \bl -> mkEncodeDecodeLE ascii85 $ BS.pack bl
  ]
