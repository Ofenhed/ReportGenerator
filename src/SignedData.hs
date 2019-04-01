-- {-# LANGUAGE ScopedTypeVariables, TypeApplications, UndecidableInstances, FlexibleInstances, AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables, QuantifiedConstraints #-}
module SignedData (SignedData, signData, getSignedData, verifySignedData) where

import Crypto.Hash (Digest, digestFromByteString, hashDigestSize, HashAlgorithm)
import Crypto.MAC.HMAC (hmacGetDigest, finalize, update)
import Data.Maybe (mapMaybe)
import Data.ByteArray ()
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as Hex

data SignedData a h = Signed a (Digest h)
data UnverifiedSignedData h = UnverifiedSigned String (Digest h)

signData hmac d = let toSign = C8.pack $ show d
                    in Signed d (hmacGetDigest $ finalize $ update hmac toSign)

getSignedData (Signed d _) = d

verifySignedData hmac (UnverifiedSigned s h) = if (hmacGetDigest $ finalize $ update hmac $ C8.pack s) == h
                                                  then case reads s of
                                                         [(s', [])] -> Just $ Signed s' h
                                                         _ -> Nothing
                                                  else Nothing

instance (Show a, HashAlgorithm h) => Show (SignedData a h) where
  show (Signed t d) = show d ++ show t

instance HashAlgorithm h => Read (UnverifiedSignedData h) where
  readsPrec l [] = []
  readsPrec l x = let takeHash :: HashAlgorithm a => a -> (Maybe (Digest a), String)
                      takeHash alg = let (hash,rest) = splitAt (2 * hashDigestSize alg) x
                                         (hex, _) = Hex.decode $ C8.pack hash
                                       in (digestFromByteString $ hex, rest)
                      (hash, rest) = takeHash undefined
                    in case hash of
                         Just d' -> [(UnverifiedSigned rest d', [])]
                         Nothing -> []
