{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Transit (
    Transit(..)
  , Transitable
  , fromTransit
  , toTransit
  , _TransitUUID
  , _TransitString
  , _TransitArray
) where

import Prelude

import Control.Lens

import Data.Bits
import Data.ByteString as B
import Data.Int
import qualified Data.Map as M
import Data.MessagePack as MP
import Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.UUID as UU

data Transit = TransitNil
             | TransitString    Text
             | TransitBool      Bool
             | TransitInt64     Int64
             | TransitFloat     Double
             | TransitBytes     B.ByteString
             | TransitKeyword   Text
             | TransitSymbol    Text
             | TransitDecimal   Rational
             | TransitInteger   Integer
             | TransitUUID      UU.UUID
             | TransitArray     [Transit]
             | TransitMap       (M.Map Transit Transit)
             | TransitExt       !Int8 B.ByteString
  deriving (Eq, Ord, Show)
             -- TODO: timestamps, URI, quoted values
             -- TODO: set, list, link, map with composite keys
makePrisms ''Transit

class Transitable a where
  fromTransit :: Transit -> a
  toTransit :: a -> Transit

bi :: (a -> b) -> (a, a) -> (b, b)
bi f (x, y) = (f x, f y)

uuid64s :: UU.UUID -> (Word64, Word64)
uuid64s u = (hi64, lo64)
  where
    (hihi, hilo, lohi, lolo) = toWords u
    hi64 = (fromIntegral hihi `shiftL` 32) + fromIntegral hilo
    lo64 = (fromIntegral lohi `shiftL` 32) + fromIntegral lolo


msgpackInteger :: (Integral a, Bits a) => a -> MP.Object
msgpackInteger i | i > (1 `shiftL` 63) = MP.ObjectInt . negate . twosComplement . fromIntegral $ i
                 | otherwise = MP.ObjectUInt $ fromIntegral i
  where
    bmask = (2 `shiftL` 64 - 1)
    twosComplement n = -(n .&. bmask) + (n .&. complement bmask)

uuidToMsgpack :: UU.UUID -> MP.Object
uuidToMsgpack u = MP.ObjectArray [MP.ObjectString "~#u", MP.ObjectArray [hi, lo]]
  where
    (hi, lo) = bi msgpackInteger $ uuid64s u

splitWord :: Word64 -> (Word32, Word32)
splitWord w64 = (hi, lo)
  where
    hi = fromIntegral $ shiftR w64 32
    lo = fromIntegral $ w64 .&. ((1 `shiftL` 32) - 1)

msgpackToUUID :: (Integral a, Integral b) => a -> b -> UU.UUID
msgpackToUUID hi64 lo64 = UU.fromWords hihi hilo lohi lolo
  where
    (hihi, hilo) = splitWord $ fromIntegral hi64
    (lohi, lolo) = splitWord $ fromIntegral lo64

transitFromString :: Text -> Transit
transitFromString (T.stripPrefix "~:" -> Just k) = TransitKeyword k
transitFromString (T.stripPrefix "~$" -> Just k) = TransitKeyword k
transitFromString txt = TransitString txt

transitFromArray :: [MP.Object] -> Transit
transitFromArray [MP.ObjectString "~#u", MP.ObjectArray [MP.ObjectUInt hi64, MP.ObjectUInt lo64]] = TransitUUID $ msgpackToUUID hi64 lo64
transitFromArray [MP.ObjectString "~#u", MP.ObjectArray [MP.ObjectInt hi64, MP.ObjectInt lo64]] = TransitUUID $ msgpackToUUID hi64 lo64
transitFromArray [MP.ObjectString "~#u", MP.ObjectArray [MP.ObjectInt hi64, MP.ObjectUInt lo64]] = TransitUUID $ msgpackToUUID hi64 lo64
transitFromArray [MP.ObjectString "~#u", MP.ObjectArray [MP.ObjectUInt hi64, MP.ObjectInt lo64]] = TransitUUID $ msgpackToUUID hi64 lo64
transitFromArray obs = TransitArray $ fmap toTransit obs

instance Transitable MP.Object where
  toTransit MP.ObjectNil        = TransitNil
  toTransit (MP.ObjectUInt i)   = TransitInteger $ fromIntegral i
  toTransit (MP.ObjectInt i)    = TransitInt64 i
  toTransit (MP.ObjectBool b)   = TransitBool b
  toTransit (MP.ObjectFloat f)  = TransitFloat $ realToFrac f
  toTransit (MP.ObjectDouble f) = TransitFloat f
  toTransit (ObjectString s)    = transitFromString $ Enc.decodeUtf8 s
  toTransit (ObjectBinary bs)   = TransitBytes bs
  toTransit (ObjectExt i bs)    = TransitExt i bs
  toTransit (ObjectArray obs)   = transitFromArray obs
  toTransit (ObjectMap m)       = TransitMap . M.fromList . fmap (bi toTransit) . M.toList $ m

  fromTransit TransitNil         = MP.ObjectNil
  fromTransit (TransitInteger i) = msgpackInteger i
  fromTransit (TransitInt64 i)   = msgpackInteger i
  fromTransit (TransitBool b)    = MP.ObjectBool b
  fromTransit (TransitFloat f)   = MP.ObjectDouble $ realToFrac f
  fromTransit (TransitDecimal f)  = MP.ObjectDouble $ realToFrac f
  fromTransit (TransitArray obs) = MP.ObjectArray $ fmap fromTransit obs
  fromTransit (TransitMap m)     = MP.ObjectMap . M.fromList . fmap (bi fromTransit) . M.toList $ m
  fromTransit (TransitString t)  = MP.ObjectString $ Enc.encodeUtf8 t
  fromTransit (TransitBytes bs)  = MP.ObjectBinary bs
  fromTransit (TransitKeyword k) = MP.ObjectString $ Enc.encodeUtf8 ("~:" `T.append` k)
  fromTransit (TransitSymbol s)  = MP.ObjectString $ Enc.encodeUtf8 ("~$" `T.append` s)
  fromTransit (TransitUUID u)    = uuidToMsgpack u
  fromTransit (TransitExt i bs)  = MP.ObjectExt i bs


{-
msgpackToTransit :: MP.Object -> Transit
msgpackToTransit = toTransit
-}

{-transitToMsgpack :: Transit -> MP.Object-}
{-transitToMsgpack = fromTransit-}

{-
fromFile :: FilePath -> IO ()
fromFile f = do
  content <- B.readFile f
  case decode content of
    Left err -> Prelude.putStrLn $ "ERROR: " ++ err
    Right obj -> print $ msgpackToTransit obj
-}
