module Codec.Base85
       ( rfc1924
       , rfc1924Alpha
       , ascii85
       , ascii85Alpha
       , encodeWord32
       , chunkToWord32BE
       , chunkToWord32LE
       , encodeChunkBE
       , encodeChunkLE
       , encodeBE
       , encodeLE
       , encode
       , toWord32
       , unWord32
       ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed as U
import qualified Data.Map as M
import Data.Int
import Data.Bits
import Data.Word
import Data.Char

fromChar :: Char -> Word8
fromChar = fromIntegral . ord

fromString :: String -> [Word8]
fromString = map fromChar

-- | Lookup-structure that defines a Base85 alphabet
type Base85Alphabet = U.Vector Word8
-- | Lookup-structure to decode a Base85 alphabet
type Base85Table = M.Map Word8 Word8

-- |the content of the RFC1924 alphabet (See <http://tools.ietf.org/html/rfc1924>)
rfc1924 :: [Word8]
rfc1924 = fromString $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ rest
  where rest = "!#$%&()*+-;<=>?@^_`{|}~"

decodingTable :: [Word8] -> Base85Table
decodingTable = M.fromList . (flip zip) [0..]

-- |the RFC1924 Base85 alphabet (See <http://tools.ietf.org/html/rfc1924>)
rfc1924Alpha :: Base85Alphabet
rfc1924Alpha = U.fromList rfc1924

-- |the ascii85 alphabet content (chars 33 to 117 in ascii !-O)
ascii85 :: [Word8]
ascii85 = [33..117]

-- |the ascii85 alphabet (chars 33 to 117 in ascii !-O)
ascii85Alpha :: Base85Alphabet
ascii85Alpha = U.fromList ascii85

ascii85Table, rfc1924Table :: Base85Table
ascii85Table = decodingTable ascii85
rfc1924Table = decodingTable rfc1924

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
chunkToWord32BE bs = toWord32 $ map getByte [0..3]
  where getByte = BS.index bs

-- |chunkToWord32LE grabs 4 Word8s from a ByteString and creates a Word32.
-- This function is not safe to use with ByteStrings with length shorter than 4.
-- Conversion is done in Little Endian
chunkToWord32LE :: BS.ByteString -> Word32
chunkToWord32LE = chunkToWord32BE . BS.reverse . BS.take 4

encodeWord32 :: Base85Alphabet -> Word32 -> BS.ByteString
encodeWord32 alpha w32 = BS.pack encoded
  where get = (alpha U.!) . fromIntegral
        chunked = [w32 `div` (lPow85 4), (w32 `div` (lPow85 3)) `rem` 85
                  , (w32 `div` (lPow85 2)) `rem` 85, (w32 `div` 85) `rem` 85
                  , w32 `rem` 85]
        encoded = map get chunked

type ChunkEncoder = BS.ByteString -> BS.ByteString
type ChunkDecoder = BS.ByteString -> BS.ByteString

encodeChunkBE :: Base85Alphabet -> ChunkEncoder
encodeChunkBE alpha = encodeWord32 alpha . chunkToWord32BE

encodeChunkLE :: Base85Alphabet -> ChunkEncoder
encodeChunkLE alpha = encodeWord32 alpha . chunkToWord32LE

groupBS :: Int64 -> BS.ByteString -> [BS.ByteString]
groupBS n bs = if BS.null bs then [] else BS.take n bs : groupBS n (BS.drop n bs)

unsafeEncode :: ChunkEncoder -> BS.ByteString -> BS.ByteString
unsafeEncode encoder bs = BS.concat groups
  where groups = map encoder $ groupBS 4 bs

encode :: ChunkEncoder -> BS.ByteString -> BS.ByteString
encode encoder bs = let pad = 4 - BS.length bs `rem` 4
                        bs' = BS.append bs (BS.replicate pad 0)
                    in unsafeEncode encoder bs'

encodeBE, encodeLE :: Base85Alphabet -> BS.ByteString -> BS.ByteString
encodeBE alpha = encode (encodeChunkBE alpha)
encodeLE alpha = encode (encodeChunkLE alpha)

toWord32 :: [Word8] -> Word32
toWord32 [b1, b2, b3, b4] =
  (b1 `shL` 24) .|. (b2 `shL` 16)  .|. (b3 `shL` 8) .|. (fromIntegral b4)
  where byte `shL` v = (fromIntegral byte) `shiftL` v
toWord32 _ = error "toWord32: need 4 Word8s to create Word32"

unWord32 :: Word32 -> [Word8]
unWord32 w32 = [shR 24, shR 16, shR 8, shR 0]
  where shR v = 0xff .&. (fromIntegral $ w32 `shiftR` v)
