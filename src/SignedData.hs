-- {-# LANGUAGE ScopedTypeVariables, TypeApplications, UndecidableInstances, FlexibleInstances, AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables, QuantifiedConstraints #-}
module SignedData (SignedData, signData, getSignedData) where

import Crypto.Hash (Digest, digestFromByteString, hashDigestSize, HashAlgorithm)
import Crypto.MAC.HMAC (hmacGetDigest, finalize, update)
import Data.Maybe (mapMaybe)
import Data.ByteArray ()
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as Hex

data SignedData a h = Signed a (Digest h)

signData hmac d = let toSign = C8.pack $ show d
                    in Signed d (hmacGetDigest $ finalize $ update hmac toSign)

getSignedData hmac (Signed d h) = if (hmacGetDigest $ finalize $ update hmac $ C8.pack $ show d) == h
                                    then Just d
                                    else Nothing

instance (Show a, HashAlgorithm h) => Show (SignedData a h) where
  show (Signed t d) = show d ++ show t

instance (Read a, HashAlgorithm h) => Read (SignedData a h) where
  readsPrec l [] = []
  readsPrec l x = let takeHash :: HashAlgorithm a => a -> (Maybe (Digest a), String)
                      takeHash alg = let (hash,rest) = splitAt (2 * hashDigestSize alg) x
                                         (hex, _) = Hex.decode $ C8.pack hash
                                       in (digestFromByteString $ hex, rest)
                      (hash, rest) = takeHash undefined
                      rest' = readsPrec l rest
                    in flip mapMaybe rest' $ \(val, trash) -> case hash of
                                                                Just d' -> Just (Signed val d', trash)
                                                                Nothing -> Nothing
