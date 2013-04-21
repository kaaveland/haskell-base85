module Codec.Base85
       (
         -- | RFC1924 Base85 alphabet.
         -- (See <http://tools.ietf.org/html/rfc1924>)
         rfc1924
         -- | The encoding and decoding tables for the RFC1924 alphabet.
       , rfc1924Alphabet
         -- | The ASCII85 alphabet for base85 encoding, chars ! to O.
       , ascii85
         -- | The encoding and decoding tables for ASCII85.
       , ascii85Alphabet
         -- | Construct a Base85 used for encoding / decoding. This fails if
         -- the alphabet does not contain 85 unique bytes.
       , makeEncoder
         -- | A container for encoding and decoding tables for Base85.
       , Base85
         -- | Retrieves a decoding table for Base85.
       , decoding
         -- | Retrieves an encoding table for Base85.
       , encoding
         -- | Encode a ByteString using Big Endian (This is standard).
       , encodeBE
         -- | Decode a ByteString using Big Endian (This is standard).
       , decodeBE
         -- | Encode a ByteString using Little Endian (This is *not* standard).
       , encodeLE
         -- | Decode a ByteString using Little Endian (This is *not* standard).
       , decodeLE) where

import Codec.Base85.Internal
import Data.List
import Data.Word

makeEncoder :: [Word8] -> Maybe Base85
makeEncoder bytes
  | length bytes /= length (nub bytes) ||
    length bytes /= 85 = Nothing
  | otherwise = return $ fromAlphabetInternal bytes
