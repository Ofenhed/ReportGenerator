-- {-# LANGUAGE ScopedTypeVariables, TypeApplications, UndecidableInstances, FlexibleInstances, AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables, QuantifiedConstraints #-}
module SignedData where

import Crypto.Hash (Digest, digestFromByteString, hashDigestSize, HashAlgorithm)
import Data.Maybe (mapMaybe)
import Data.ByteArray ()
import qualified Data.Text             as Text
import qualified Data.ByteString.Char8 as C8

data SignedData a = Signed Text.Text (Digest a)

instance HashAlgorithm a => Show (SignedData a) where
  show (Signed t d) = show d ++ Text.unpack t

instance HashAlgorithm a => Read (SignedData a) where
  readsPrec l [] = []
  readsPrec l x = let takeHash :: HashAlgorithm a => a -> (Maybe (Digest a), String)
                      takeHash alg = let (hash,rest) = splitAt (hashDigestSize alg) x
                                       in (digestFromByteString $ C8.pack hash, rest)
                      (hash, rest) = takeHash undefined
                      rest' = readsPrec l rest
                    in flip mapMaybe rest' $ \(val, trash) -> case hash of
                                                                Just d' -> Just (Signed (Text.pack val) d', trash)
                                                                Nothing -> Nothing
