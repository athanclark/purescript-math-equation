module Test.Main where

import Math.Equation

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Console (log, warn)
import Effect.Exception (try)
import Effect.Unsafe (unsafePerformEffect)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Class
  (class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength, encodeArrayBuffer, decodeArrayBuffer)
import Test.QuickCheck (Result (Failed), (===), quickCheck')
import Unsafe.Coerce (unsafeCoerce)



main :: Effect Unit
main = do
  log "Json encodings"
  log "  NumberConstant"
  quickCheck' 1000 (jsonIso :: NumberConstant -> Result)
  log "  Value"
  quickCheck' 1000 (jsonIso :: Value Number -> Result)
  log "  NumberValue"
  quickCheck' 1000 (jsonIso :: NumberValue -> Result)
  log "ArrayBuffer encodings"
  log "  NumberConstant"
  quickCheck' 1000 (arrayBufferIso :: NumberConstant -> Result)



jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => Show a => a -> Result
jsonIso x = Right x === decodeJson (encodeJson x)


arrayBufferIso :: forall a
                . Show a
               => Eq a
               => EncodeArrayBuffer a
               => DecodeArrayBuffer a
               => DynamicByteLength a
               => a -> Result
arrayBufferIso x = unsafePerformEffect do
  eX <- try do
    b <- encodeArrayBuffer x
    mY <- decodeArrayBuffer b
    case mY of
      Nothing -> pure (Failed "Nothing")
      Just y -> pure (x === y)
  case eX of
    Left e -> do
      warn (unsafeCoerce x)
      pure (Failed (show e))
    Right x -> pure x
