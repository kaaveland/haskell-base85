-- |
-- Module : Codec.Base85
--
-- License : BSD-style
-- Maintainer : kaaveland@gmail.com
-- Stability : experimental
--
-- Simple and easy-to-use base85 encoding and decoding of bytestrings.
--
-- For the time being, the decoding functions do not anticipate characters
-- that are not in the encoding alphabets and will decode strings containing
-- whitespace wrongly.
--
-- Functions with a name ending in BE do Big Endian byte-ordering wherever
-- conversion from 32 bit values to 8 bit values or the other way are needed.
--
-- Conversely, functions ending with LE do Little Endian byte-ordering.
--
-- This module mainly reimports from "Codec.Base85.Internal"
module Codec.Base85
       (
         -- * The @Base85@ type.
         Base85
       , Base85Alphabet
       , Base85Table
       , decoding
       , encoding
         -- * Creating encoders
         -- | An alphabet is valid if it contains exactly 85 unique bytes.
         -- No checking is done to verify that they are printable
         -- or all in ASCII. These functions can therefore be used to implement
         -- alphabets that use extended-ascii.
       , makeEncoder
       , makeEncoderFromString
       , makeAsciiEncoderFromString
         -- * Alphabets and standard encoders
       , rfc1924
       , rfc1924Alphabet
       , ascii85
       , ascii85Alphabet
         -- * Standard encoding
         -- | It is standard to perform Big Endian conversion while encoding
         -- or decoding. 
       , encodeBE
       , decodeBE
       , encodeAscii85
       , decodeAscii85
       , encodeRFC1924
       , decodeRFC1924
         -- * Non-standard encoding
         -- | Using Little Endian is non-standard and is here only because it
         -- was pretty easy to add.
       , encodeLE
       , decodeLE) where

import Codec.Base85.Internal
import Data.List
import Data.Word
import Data.Char
import qualified Data.ByteString.Lazy as BS

-- | Create a new 'Base85' from an alphabet.
-- This checks that the alphabet is valid.
makeEncoder :: [Word8] -> (BS.ByteString -> BS.ByteString) -> Maybe Base85
makeEncoder bytes zeroExpander
  | length bytes /= length (nub bytes) ||
    length bytes /= 85 = Nothing
  | otherwise = return $ fromAlphabetInternal bytes zeroExpander
-- | Create a new 'Base85' from an alphabet.
-- This checks that the alphabet is valid.
makeEncoderFromString :: String -> (BS.ByteString -> BS.ByteString) -> Maybe Base85
makeEncoderFromString alphabet = makeEncoder (fromString alphabet)

-- | Create a new 'Base85' from an alphabet that contains only
-- ascii bytes and only printable characters.
makeAsciiEncoderFromString :: String -> (BS.ByteString -> BS.ByteString) -> Maybe Base85
makeAsciiEncoderFromString alphabet zeroExpander
  | all isAscii alphabet && all isPrint alphabet =
    makeEncoderFromString alphabet zeroExpander
  | otherwise = Nothing

-- | Standard ascii85 decoding.
decodeAscii85 :: BS.ByteString -> BS.ByteString
decodeAscii85 = decodeBE ascii85

-- | Standard ascii85 encoding.
encodeAscii85 :: BS.ByteString -> BS.ByteString
encodeAscii85 = encodeBE ascii85

-- | Standard rfc1924 encoding.
encodeRFC1924 :: BS.ByteString -> BS.ByteString
encodeRFC1924 = encodeBE rfc1924

-- | Standard rfc1924 decoding.
decodeRFC1924 :: BS.ByteString -> BS.ByteString
decodeRFC1924 = decodeBE rfc1924
