module Aradia.Utils where

import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format
import Data.Text.Format.Params(Params)

import Data.Random.Source.PureMT

-- | A wrapper around `format` that returns a strict Text
format' :: Params ps => Format -> ps -> Text
format' f ps = TL.toStrict (format f ps)

-- |Uses SHA1 to compute a hash for PureMT
pureMTFromText :: Text -> PureMT
pureMTFromText = pureMT . toWord64 . SHA1.hash . encodeUtf8
 where
   toWord64 = B.foldl' (\x y -> x * 256 + fromIntegral y) 0 . B.take 4

-- |Construct a bar with granularity n
asBars :: Int -> Double -> Text
asBars n prob | prob < 0 || prob > 1 = "Error when making bar: probability out of range"
              | otherwise = format' "[{}{}]" (T.replicate prob' "#", T.replicate rem "-")
  where
    prob' = round (prob * fromIntegral n)
    rem = n - prob'
