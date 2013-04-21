-- |
-- Module : Codec.Base85.Internal
-- License : BSD-style
-- Maintainer : kaaveland@gmail.com
-- Stability : experimental
--
-- This module does all the heavy lifting for Codec.Base85.
--
-- /Naming conventions/
--
-- Functions with a name ending in BE do Big Endian byte-ordering wherever
-- conversion from 32 bit values to 8 bit values or the other way are needed.
--
-- Conversely, functions ending with LE do Little Endian byte-ordering.

module Codec.Base85.Internal
       (
         -- * @Base85@
         Base85Alphabet
       , Base85Table
       , Base85(..)
         -- * Alphabets
       , rfc1924         
       , rfc1924Alphabet
       , ascii85
       , ascii85Alphabet
       , fromString
       , fromAlphabetInternal
         -- * Byte and byte sequence manipulation
       , toWord32BE
       , unWord32BE
       , toWord32LE
       , unWord32LE
         -- * Low level encoding and decoding
       , encodeWord32
       , chunkToWord32BE
       , chunkToWord32LE
       , encodeChunkBE
       , decodeChunkBE
       , encodeChunkLE
       , decodeChunkLE
         -- * High level encoding and decoding
       , encodeBE
       , encodeLE
       , encode
       , decode
       , decodeBE
       , decodeLE
       , removeAllWhiteSpace
       ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Unboxed as U
import Data.Int
import Data.Bits
import Data.Word
import Data.Char
import Data.List

-- | This is used for creating alphabets.
fromChar :: Char -> Word8
fromChar = fromIntegral . ord
-- | This is used for creating alphabets.
fromString :: String -> [Word8]
fromString = map fromChar

-- | Used for encoding from bytes to base85.
type Base85Alphabet = U.Vector Word8
-- | Used for decoding from base85 to bytes.
type Base85Table = U.Vector Word8

-- | Internally this contains one unboxed vector for each direction of
-- translation but this may change.
data Base85 = Base85 {
  -- | Retrieve the alphabet being used for encoding.
  encoding :: Base85Alphabet
  -- | Retrieve the table being used for decoding.
  , decoding :: Base85Alphabet
  -- | Exands zeros in the input string to groups of zeros before decoding.
  , expandZeros :: BS.ByteString -> BS.ByteString}
            
-- | Given an alphabet of characters, sets up encoding and decoding tables.
-- This does no error-checking.
fromAlphabetInternal :: [Word8] -> (BS.ByteString -> BS.ByteString) -> Base85
fromAlphabetInternal alpha = Base85 enc dec
  where enc = U.fromList alpha
        dec = decodingTable alpha

-- | The ASCII85 alphabet for base85 encoding, chars ! to O.
ascii85Alphabet :: [Word8]
ascii85Alphabet = [33..117]

-- | The encoding and decoding tables for ASCII85.
ascii85 :: Base85
ascii85 = fromAlphabetInternal ascii85Alphabet expandZeros
  where expandZeros = BS.intercalate excls . BS.split z
        z = fromIntegral $ ord 'z'
        excls = BS.replicate 5 excl
        excl = fromIntegral $ ord '!'

-- | RFC1924 Base85 alphabet.
-- (See <http://tools.ietf.org/html/rfc1924>)
rfc1924Alphabet :: [Word8]
rfc1924Alphabet = fromString $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ rest
  where rest = "!#$%&()*+-;<=>?@^_`{|}~"

-- | The encoding and decoding tables for the RFC1924 alphabet.
rfc1924 :: Base85
rfc1924 = fromAlphabetInternal rfc1924Alphabet id

-- | Given an alphabet, this creates the reverse lookup-table.
decodingTable :: [Word8] -> Base85Table
decodingTable alpha = U.generate 255 lookFor
  where lookFor i = case elemIndex (fromIntegral i) alpha of
          Nothing -> 0 :: Word8
          (Just w8) -> fromIntegral w8 :: Word8

-- | A cache of some of the low powers of 8, for efficiency.
lowPowersOf85 :: U.Vector Word32
lowPowersOf85 = U.fromList $ map toW32 $ zipWith (^) (repeat 85) ([0..8] :: [Int])
  where toW32 :: Int -> Word32
        toW32 = fromIntegral

-- | Take a low power of 8 from the cache.
lPow85 :: Word32 -> Word32
lPow85 = (lowPowersOf85 U.!) . fromIntegral

-- | Take the first 4 bytes of a 'BS.ByteString' and make it a 'Word32'.
-- This does no error-checking and will cause an error if passed less than 4
-- bytes. This function uses Big Endian conversion.
chunkToWord32BE :: BS.ByteString -> Word32
chunkToWord32BE bs = toWord32BE $ map getByte [0..3]
  where getByte = BS.index bs

-- | Take the first 4 bytes of a 'BS.ByteString' and make it a 'Word32'.
-- This does no error-checking and will cause an error if passed less than 4
-- bytes. This function uses Little Endian conversion.
chunkToWord32LE :: BS.ByteString -> Word32
chunkToWord32LE = chunkToWord32BE . BS.reverse . BS.take 4

-- | Turns a 'Word32' into a 'BS.ByteString' using the alphabet that is
-- provided.
encodeWord32 :: Base85Alphabet -> Word32 -> BS.ByteString
encodeWord32 alpha w32 = BS.pack encoded
  where get = (alpha U.!) . fromIntegral
        chunked = [w32 `div` lPow85 4, (w32 `div` lPow85 3) `rem` 85
                  , (w32 `div` lPow85 2) `rem` 85, (w32 `div` 85) `rem` 85
                  , w32 `rem` 85]
        encoded = map get chunked

-- | Alias for a function that can encode a chunk of 4 'Word8'.
type ChunkEncoder = BS.ByteString -> BS.ByteString
-- | Alias for a function that can decode a chunk of 5 'Word8'
type ChunkDecoder = BS.ByteString -> BS.ByteString

-- | Using the provided 'Base85', encode one chunk of 4 bytes. This
-- will create 5 bytes in Big Endian.
-- It is an error to pass less than 4 bytes to this function.
encodeChunkBE :: Base85 -> ChunkEncoder
encodeChunkBE alpha = encodeWord32 (encoding alpha) . chunkToWord32BE

-- | Using the provided 'Base85', encode one chunk of 4 bytes. This
-- will create 5 bytes in Little Endian.
-- It is an error to pass less than 4 bytes to this function.
encodeChunkLE :: Base85 -> ChunkEncoder
encodeChunkLE alpha = encodeWord32 (encoding alpha) . chunkToWord32LE

-- | Group a 'BS.ByteString' into chunks of 'Int64' bytes each.
-- This is a naive recursive solution and can probably be improved.
groupBS :: Int64 -> BS.ByteString -> [BS.ByteString]
groupBS n bs = if BS.null bs then [] else BS.take n bs : groupBS n (BS.drop n bs)

-- | Unsafely apply 'ChunkEncoder' to all chunks of 4 'Word8' in the
-- provided 'BS.ByteString'. This will fail if length of the 'BS.ByteString'
-- is not a multiple of 4. Don't call this directly.
unsafeEncode :: ChunkEncoder -> BS.ByteString -> BS.ByteString
unsafeEncode encoder bs = BS.concat groups
  where groups = map encoder $ groupBS 4 bs

-- | Safely apply 'ChunkEncoder' to all chunks of 4 'Word8' in the
-- provided 'BS.ByteString'. This function pads the inputstring as needed
-- to ensure that a multiple of 4 bytes is used. The padding is removed
-- before the result is returned.
encode :: ChunkEncoder -> BS.ByteString -> BS.ByteString
encode encoder bs = let pad = 4 - BS.length bs `rem` 4
                        bs' = BS.append bs (BS.replicate pad 0)
                        encoded = unsafeEncode encoder bs'
                    in BS.take (BS.length encoded - pad) encoded

-- | This performs safe encoding using the provided Base85 tables.
encodeBE :: Base85 -> BS.ByteString -> BS.ByteString
encodeBE b85 = encode $ encodeChunkBE b85
-- | This performs safe encoding using the provided Base85 tables.
encodeLE :: Base85 -> BS.ByteString -> BS.ByteString
encodeLE b85 = encode $ encodeChunkLE b85

-- | This creates a 'Word32' from 4 'Word8'. It is an error to pass
-- a list containing a different number of bytes than 4. This function
-- uses Big Endian.
toWord32BE :: [Word8] -> Word32
toWord32BE [b1, b2, b3, b4] =
  (b1 `shL` 24) .|. (b2 `shL` 16)  .|. (b3 `shL` 8) .|. fromIntegral b4
  where byte `shL` v = fromIntegral byte `shiftL` v
toWord32BE _ = error "toWord32BE: need 4 Word8s to create Word32"

-- | This creates a 'Word32' from 4 'Word8'. It is an error to pass
-- a list containing a different number of bytes than 4. This function
-- uses Little Endian.
toWord32LE :: [Word8] -> Word32
toWord32LE = toWord32BE . reverse

-- | This splits a 'Word32' into its constituent 'Word8' bytes.
-- The result is Big Endian.
unWord32BE :: Word32 -> [Word8]
unWord32BE w32 = [shR 24, shR 16, shR 8, shR 0]
  where shR v = 0xff .&. (fromIntegral $ w32 `shiftR` v)
-- | This splits a 'Word32' into its constituent 'Word8' bytes.
-- The result is Little Endian.        
unWord32LE :: Word32 -> [Word8]
unWord32LE = reverse . unWord32BE

-- | This calculates the Base85 value of the 'Word8's in 'BS.ByteString'.
word32Chunk :: BS.ByteString -> Word32
word32Chunk = BS.foldl (\w32 w8 -> w32 * 85 + fromIntegral w8) 0

-- | This calculates the 'Word32' value of base85-encoded data which
-- was encoded using the provided 'Base85'.
decodeChunk :: Base85 -> BS.ByteString -> Word32
decodeChunk table bs = word32Chunk w8s
  where w8s = BS.map get bs
        get = (decoding table U.!) . fromIntegral

-- | Using Big Endian and the provided 'Base85', decode one chunk of 5
-- bytes into a 'BS.ByteString'
decodeChunkBE :: Base85 -> BS.ByteString -> BS.ByteString
decodeChunkBE table = BS.pack . unWord32BE . decodeChunk table

-- | Using Little Endian and the provided 'Base85', decode one chunk of
-- 5 bytes into ' BS.ByteString'
decodeChunkLE :: Base85 -> BS.ByteString -> BS.ByteString
decodeChunkLE table = BS.pack . unWord32LE . decodeChunk table

-- | Unsafely apply 'ChunkDecoder' to every chunk of 5 bytes in the
-- input 'BS.ByteString'. This will fail if the length of the input
-- is not a multiple of 5. It does not expand zeros. Do not call this directly.
unsafeDecode :: ChunkDecoder -> BS.ByteString -> BS.ByteString
unsafeDecode decoder bs = BS.concat groups
  where groups = map decoder $ groupBS 5 bs

-- | This removes all whitespace from an encoded Base85 bytestring.
-- This is to ensure that it doesn't get garbled in decoding, as whitespace
-- is not valid in a base85 alphabet.
removeAllWhiteSpace :: BS.ByteString -> BS.ByteString
removeAllWhiteSpace = BS.filter (not . isBlank)
  where isBlank = isSpace . chr . fromIntegral

-- | Safely apply 'ChunkDecoder' to every chunk of 5 bytes in the
-- input 'BS.ByteString'. This function pads the input until its
-- length is a multiple of 5 before decoding. The padding is removed
-- before the function returns.
--
-- Padding is always done using the last valid byte of the provided
-- alphabet. This expands zeros using the expandZeros function of the
-- provided 'Base85'.
decode :: Base85 -> ChunkDecoder -> BS.ByteString -> BS.ByteString
decode tab decoder bs = let expanded = expandZeros tab bs
                            safeBS = removeAllWhiteSpace expanded
                            pad = 5 - BS.length safeBS `rem` 5
                            padItem = U.last $ encoding tab
                            bs' = BS.append safeBS (BS.replicate pad padItem)
                            decoded = unsafeDecode decoder bs'
                        in BS.take (BS.length decoded - pad) decoded

-- | This performs standard, Big Endian decoding using the provided 'Base85'
-- instance.
decodeBE :: Base85 -> ChunkDecoder
decodeBE table = decode table $ decodeChunkBE table

-- | This performs non-standard, Little Endian decoding using the provided 'Base85'
-- instance.
decodeLE :: Base85 -> ChunkDecoder
decodeLE table = decode table $ decodeChunkLE table
