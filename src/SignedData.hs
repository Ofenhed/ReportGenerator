-- {-# LANGUAGE ScopedTypeVariables, TypeApplications, UndecidableInstances, FlexibleInstances, AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables, QuantifiedConstraints #-}
module SignedData where

import Crypto.Hash (Digest, digestFromByteString, hashDigestSize, HashAlgorithm)
import Data.Maybe (mapMaybe)
import Data.ByteArray ()
import qualified Data.ByteString.Char8 as C8

data SignedData a h = Signed a (Digest h)

instance (Show a, HashAlgorithm h) => Show (SignedData a h) where
  show (Signed t d) = show d ++ show t

instance (Read a, HashAlgorithm h) => Read (SignedData a h) where
  readsPrec l [] = []
  readsPrec l x = let takeHash :: HashAlgorithm a => a -> (Maybe (Digest a), String)
                      takeHash alg = let (hash,rest) = splitAt (hashDigestSize alg) x
                                       in (digestFromByteString $ C8.pack hash, rest)
                      (hash, rest) = takeHash undefined
                      rest' = readsPrec l rest
                    in flip mapMaybe rest' $ \(val, trash) -> case hash of
                                                                Just d' -> Just (Signed val d', trash)
                                                                Nothing -> Nothing
