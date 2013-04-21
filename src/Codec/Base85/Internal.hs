module Codec.Base85.Internal
       ( rfc1924
       , rfc1924Alphabet
       , ascii85
       , ascii85Alphabet
       , Base85(..)
       , encodeWord32
       , chunkToWord32BE
       , chunkToWord32LE
       , encodeChunkBE
       , decodeChunkBE
       , encodeChunkLE
       , decodeChunkLE
       , encodeBE
       , encodeLE
       , encode
       , decode
       , decodeBE
       , decodeLE
       , toWord32BE
       , unWord32BE
       , toWord32LE
       , unWord32LE
       , Base85Alphabet
       , Base85Table
       , fromAlphabetInternal
       ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed as U
import Data.Int
import Data.Bits
import Data.Word
import Data.Char
import Data.List

fromChar :: Char -> Word8
fromChar = fromIntegral . ord

fromString :: String -> [Word8]
fromString = map fromChar

-- | Lookup-structure that defines a Base85 alphabet
type Base85Alphabet = U.Vector Word8
-- | Lookup-structure to decode a Base85 alphabet
type Base85Table = U.Vector Word8

data Base85 = Base85 { encoding :: Base85Alphabet, decoding :: Base85Alphabet}
            deriving (Show, Eq)

fromAlphabetInternal :: [Word8] -> Base85
fromAlphabetInternal alpha = Base85 enc dec
  where enc = U.fromList alpha
        dec = decodingTable alpha

-- |the ascii85 alphabet content (chars 33 to 117 in ascii !-O)
ascii85Alphabet :: [Word8]
ascii85Alphabet = [33..117]

ascii85 :: Base85
ascii85 = fromAlphabetInternal ascii85Alphabet

-- |the RFC1924 Base85 alphabet (See <http://tools.ietf.org/html/rfc1924>)
rfc1924Alphabet :: [Word8]
rfc1924Alphabet = fromString $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ rest
  where rest = "!#$%&()*+-;<=>?@^_`{|}~"

rfc1924 :: Base85
rfc1924 = fromAlphabetInternal rfc1924Alphabet

decodingTable :: [Word8] -> Base85Table
decodingTable alpha = U.generate 255 lookFor
  where lookFor i = case elemIndex (fromIntegral i) alpha of
          Nothing -> 0 :: Word8
          (Just w8) -> fromIntegral w8 :: Word8

lowPowersOf85 :: U.Vector Word32
lowPowersOf85 = U.fromList $ map toW32 $ zipWith (^) (repeat 85) ([0..8] :: [Int])
  where toW32 :: Int -> Word32
        toW32 = fromIntegral

lPow85 :: Word32 -> Word32
lPow85 = (lowPowersOf85 U.!) . fromIntegral

-- |chunkToWord32 grabs 4 Word8s from a ByteString and creates a Word32.
-- This function is not safe to use with ByteStrings with length shorter than 4.
-- Conversion is done in Big Endian
chunkToWord32BE :: BS.ByteString -> Word32
chunkToWord32BE bs = toWord32BE $ map getByte [0..3]
  where getByte = BS.index bs

-- |chunkToWord32LE grabs 4 Word8s from a ByteString and creates a Word32.
-- This function is not safe to use with ByteStrings with length shorter than 4.
-- Conversion is done in Little Endian
chunkToWord32LE :: BS.ByteString -> Word32
chunkToWord32LE = chunkToWord32BE . BS.reverse . BS.take 4

-- |encodeWord32 uses the provided alphabet to encode a Word32 into 5 Word8s.
encodeWord32 :: Base85Alphabet -> Word32 -> BS.ByteString
encodeWord32 alpha w32 = BS.pack encoded
  where get = (alpha U.!) . fromIntegral
        chunked = [w32 `div` (lPow85 4), (w32 `div` (lPow85 3)) `rem` 85
                  , (w32 `div` (lPow85 2)) `rem` 85, (w32 `div` 85) `rem` 85
                  , w32 `rem` 85]
        encoded = map get chunked

type ChunkEncoder = BS.ByteString -> BS.ByteString
type ChunkDecoder = BS.ByteString -> BS.ByteString

-- |encodeChunkBE uses the provided alphabet to encode a chunk of 4 Word8s.
-- It uses Big Endian.
encodeChunkBE :: Base85 -> ChunkEncoder
encodeChunkBE alpha = encodeWord32 (encoding alpha) . chunkToWord32BE

-- |encodeChunkLE uses the provided alphabet to encode a chunk of 4 Word8s.
-- It uses Little Endian.
encodeChunkLE :: Base85 -> ChunkEncoder
encodeChunkLE alpha = encodeWord32 (encoding alpha) . chunkToWord32LE

groupBS :: Int64 -> BS.ByteString -> [BS.ByteString]
groupBS n bs = if BS.null bs then [] else BS.take n bs : groupBS n (BS.drop n bs)

unsafeEncode :: ChunkEncoder -> BS.ByteString -> BS.ByteString
unsafeEncode encoder bs = BS.concat groups
  where groups = map encoder $ groupBS 4 bs

-- \encode uses the provided encoder to encode all chunks of 4 Word8s into chunks of
--  5 Word8s that are then concatenated.
encode :: ChunkEncoder -> BS.ByteString -> BS.ByteString
encode encoder bs = let pad = 4 - BS.length bs `rem` 4
                        bs' = BS.append bs (BS.replicate pad 0)
                        encoded = unsafeEncode encoder bs'
                    in BS.take (BS.length encoded - pad) encoded

encodeBE, encodeLE :: Base85 -> BS.ByteString -> BS.ByteString
-- |encodeBE uses the provided alphabet to encode with Big Endian conversion.
encodeBE b85 = encode $ encodeChunkBE b85
-- |encodeLE uses the provided alphabet to encode with Little Endian conversion.
encodeLE b85 = encode $ encodeChunkLE b85

toWord32BE, toWord32LE :: [Word8] -> Word32
-- |toWord32BE takes a list of 4 Word8s and creates a Word32 in Big Endian.
toWord32BE [b1, b2, b3, b4] =
  (b1 `shL` 24) .|. (b2 `shL` 16)  .|. (b3 `shL` 8) .|. (fromIntegral b4)
  where byte `shL` v = (fromIntegral byte) `shiftL` v
toWord32BE _ = error "toWord32BE: need 4 Word8s to create Word32"
-- |toWord32BE takes a list of 4 Word8s and creates a Word32 in Little Endian.
toWord32LE = toWord32BE . reverse

unWord32LE, unWord32BE :: Word32 -> [Word8]
-- |unWord32BE takes a Word32 and creates a list of 4 Word8s in Big Endian
unWord32BE w32 = [shR 24, shR 16, shR 8, shR 0]
  where shR v = 0xff .&. (fromIntegral $ w32 `shiftR` v)
-- |unWord32BE takes a Word32 and creates a list of 4 Word8s in Little Endian
unWord32LE = reverse . unWord32BE

word32Chunk :: BS.ByteString -> Word32
word32Chunk = BS.foldl (\w32 w8 -> w32 * 85 + (fromIntegral w8)) 0

decodeChunk :: Base85 -> BS.ByteString -> Word32
decodeChunk table bs = word32Chunk w8s
  where w8s = BS.map get bs
        get = ((decoding table) U.!) . fromIntegral

decodeChunkLE, decodeChunkBE :: Base85 -> BS.ByteString -> BS.ByteString
-- |decodeChunkBE uses the provided table to decode 5 Word8s. It is not safe
--  to call this function with less than 5 Words8. It uses Big Endian.
decodeChunkBE table = BS.pack . unWord32BE . decodeChunk table
-- |decodeChunkBE uses the provided table to decode 5 Word8s. It is not safe
--  to call this function with less than 5 Words8. It uses Little Endian.
decodeChunkLE table = BS.pack . unWord32LE . decodeChunk table

unsafeDecode :: ChunkDecoder -> BS.ByteString -> BS.ByteString
unsafeDecode decoder bs = BS.concat groups
  where groups = map decoder $ groupBS 5 bs

decode :: Base85 -> ChunkDecoder -> BS.ByteString -> BS.ByteString
decode tab decoder bs = let pad = 5 - BS.length bs `rem` 5
                            padItem = U.last $ encoding tab
                            bs' = BS.append bs (BS.replicate pad padItem)
                            decoded = unsafeDecode decoder bs'
                        in BS.take (BS.length decoded - pad) decoded

decodeBE, decodeLE :: Base85 -> ChunkDecoder
decodeBE table = decode table $ decodeChunkBE table
decodeLE table = decode table $ decodeChunkLE table
