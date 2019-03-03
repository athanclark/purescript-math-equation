module Test.Main where

import Math.Equation

import Prelude
import Data.Either (Either (..))
import Effect (Effect)
import Effect.Console (log)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Test.QuickCheck (Result, (===), quickCheck')



main :: Effect Unit
main = do
  log "Testing encodings"
  log "  NumberConstant"
  quickCheck' 1000 (jsonIso :: NumberConstant -> Result)
  log "  Equation"
  quickCheck' 1000 (jsonIso :: Equation Number -> Result)
  log "  NumberEquation"
  quickCheck' 1000 (jsonIso :: NumberEquation -> Result)



jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => Show a => a -> Result
jsonIso x = Right x === decodeJson (encodeJson x)
