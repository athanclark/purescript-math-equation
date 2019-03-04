-- | This module defines data structures that mirror common Math functions, to
-- | ease the storage of such an equation.

module Math.Equation
  ( VarName, BoundVars, emptyVars, bindVars, bindVar, BoundError (..)
  , NumberConstant (..), NumberValue (..), Value (..), Equation
  , computeConstant, computeNumberValue, computeValue
  , genValue
  ) where

import Prelude
  ( class Eq, class Show, class Ord, class Functor
  , (<$>), (<*>), (+), (-), (*), (/), (<=), (<>), (&&)
  , negate, recip, gcd, lcm, min, max, mod, pure, map, show, bind, unit, otherwise, eq, identity)
import Data.Ord (abs)
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Foldable (class Foldable)
import Data.EuclideanRing (class EuclideanRing)
import Data.DivisionRing (class DivisionRing)
import Data.Generic.Rep (class Generic, from)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut
  (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (~>), (:=), jsonEmptyObject, (.:))
import Data.ArrayBuffer.Types (ByteLength)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , putArrayBuffer, readArrayBuffer, byteLength, Uint8 (..), Float64BE (..))
import Data.UInt (toInt, fromInt) as UInt
import Control.Alternative ((<|>))
import Control.Monad.Rec.Class (tailRecM, Step (..))
import Foreign.Object (Object)
import Foreign.Object (fromFoldable, insert, lookup) as O
import Effect (Effect)
import Effect.Exception (throw)
import Math
  ( acos, asin, atan, atan2, cos, sin, tan, ceil, floor, round, trunc, exp
  , log, pow, sqrt, remainder, e, pi, tau, ln2, ln10, log2e, log10e, sqrt1_2, sqrt2)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, genericArbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (Gen, oneOf, sized)


type VarName = String


data NumberConstant
  = E -- ^ `e`, Euler's constant
  | Ln2 -- ^ `log_e(2)`
  | Ln10 -- ^ `log_e(10)`
  | Log2E -- ^ `log_2(e)`
  | Log10E -- ^ `log_10(e)`
  | Pi
  | Tau
  | Sqrt1_2 -- ^ Square-root of `0.5`
  | Sqrt2 -- ^ Square-root of `2`

derive instance genericNumberConstant :: Generic NumberConstant _
instance eqNumberConstant :: Eq NumberConstant where
  eq = genericEq
instance showNumberConstant :: Show NumberConstant where
  show = genericShow
instance encodeJsonNumberConstant :: EncodeJson NumberConstant where
  encodeJson x =
    let s = case x of
          E -> "e"
          Ln2 -> "ln2"
          Ln10 -> "ln10"
          Log2E -> "log2e"
          Log10E -> "log10e"
          Pi -> "pi"
          Tau -> "tau"
          Sqrt1_2 -> "sqrt1_2"
          Sqrt2 -> "sqrt2"
    in  encodeJson s
instance decodeJsonNumberConstant :: DecodeJson NumberConstant where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | eq s "e" -> Right E
        | eq s "ln2" -> Right Ln2
        | eq s "ln10" -> Right Ln10
        | eq s "log2e" -> Right Log2E
        | eq s "log10e" -> Right Log10E
        | eq s "pi" -> Right Pi
        | eq s "tau" -> Right Tau
        | eq s "sqrt1_2" -> Right Sqrt1_2
        | eq s "sqrt2" -> Right Sqrt2
        | otherwise -> Left "Not a NumberConstant"
instance dynamicByteLengthNumberConstant :: DynamicByteLength NumberConstant where
  byteLength _ = pure 1
instance encodeArrayBufferNumberConstant :: EncodeArrayBuffer NumberConstant where
  putArrayBuffer b o x = putArrayBuffer b o y
    where
      y = Uint8 z
      z = UInt.fromInt q
      q = case x of
            E -> 0
            Ln2 -> 1
            Ln10 -> 2
            Log2E -> 3
            Log10E -> 4
            Pi -> 5
            Tau -> 6
            Sqrt1_2 -> 7
            Sqrt2 -> 8
instance decodeArrayBufferNumberConstant :: DecodeArrayBuffer NumberConstant where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Just (Uint8 w) -> case UInt.toInt w of
        q | eq q 0 -> pure (Just E)
          | eq q 1 -> pure (Just Ln2)
          | eq q 2 -> pure (Just Ln10)
          | eq q 3 -> pure (Just Log2E)
          | eq q 4 -> pure (Just Log10E)
          | eq q 5 -> pure (Just Pi)
          | eq q 6 -> pure (Just Tau)
          | eq q 7 -> pure (Just Sqrt1_2)
          | eq q 8 -> pure (Just Sqrt2)
          | otherwise -> throw "Not a NumberConstant"
      Nothing -> pure Nothing
instance arbitraryNumberConstant :: Arbitrary NumberConstant where
  arbitrary = genericArbitrary
instance coarbitraryNumberConstant :: Coarbitrary NumberConstant where
  coarbitrary x = coarbitrary (from x) -- genericCoarbitrary


data NumberValue
  = ACos NumberValue
  | ASin NumberValue
  | ATan NumberValue
  | ATan2 NumberValue NumberValue
  | Cos NumberValue
  | Sin NumberValue
  | Tan NumberValue
  | Ceil NumberValue -- ^ Round-up
  | Floor NumberValue -- ^ Round-down
  | Round NumberValue -- ^ Round-middle
  | Trunc NumberValue -- ^ `if x > 0 then floor(x) else ceil(x)`
  | Exp NumberValue -- ^ `e^x`
  | Log NumberValue -- ^ `log_e(x)`, natural logarithm
  | Pow NumberValue NumberValue -- ^ `x^y`
  | Sqrt NumberValue
  | Remainder NumberValue NumberValue
  | Value (Value Number)
  | Constant NumberConstant

derive instance genericNumberValue :: Generic NumberValue _
instance eqNumberValue :: Eq NumberValue where
  eq a b = case Tuple a b of
    Tuple (ACos x) (ACos y) -> eq x y
    Tuple (ASin x) (ASin y) -> eq x y
    Tuple (ATan x) (ATan y) -> eq x y
    Tuple (ATan2 x x') (ATan2 y y') -> eq x y && eq x' y'
    Tuple (Cos x) (Cos y) -> eq x y
    Tuple (Sin x) (Sin y) -> eq x y
    Tuple (Tan x) (Tan y) -> eq x y
    Tuple (Ceil x) (Ceil y) -> eq x y
    Tuple (Floor x) (Floor y) -> eq x y
    Tuple (Round x) (Round y) -> eq x y
    Tuple (Trunc x) (Trunc y) -> eq x y
    Tuple (Exp x) (Exp y) -> eq x y
    Tuple (Log x) (Log y) -> eq x y
    Tuple (Pow x x') (Pow y y') -> eq x y && eq x' y'
    Tuple (Sqrt x) (Sqrt y) -> eq x y
    Tuple (Remainder x x') (Remainder y y') -> eq x y && eq x' y'
    Tuple (Value x) (Value y) -> eq x y
    Tuple (Constant x) (Constant y) -> eq x y
    _ -> false
instance showNumberValue :: Show NumberValue where
  show a = case a of
    ACos x -> "ACos (" <> show x <> ")"
    ASin x -> "ASin (" <> show x <> ")"
    ATan x -> "ATan (" <> show x <> ")"
    ATan2 x y -> "ATan2 (" <> show x <> ") (" <> show y <> ")"
    Cos x -> "Cos (" <> show x <> ")"
    Sin x -> "Sin (" <> show x <> ")"
    Tan x -> "Tan (" <> show x <> ")"
    Ceil x -> "Ceil (" <> show x <> ")"
    Floor x -> "Floor (" <> show x <> ")"
    Round x -> "Round (" <> show x <> ")"
    Trunc x -> "Trunc (" <> show x <> ")"
    Exp x -> "Exp (" <> show x <> ")"
    Log x -> "Log (" <> show x <> ")"
    Pow x y -> "Pow (" <> show x <> ") (" <> show y <> ")"
    Sqrt x -> "Sqrt (" <> show x <> ")"
    Remainder x y -> "Remainder (" <> show x <> ") (" <> show y <> ")"
    Value x -> "Value (" <> show x <> ")"
    Constant x -> "Constant (" <> show x <> ")"
instance encodeJsonNumberValue :: EncodeJson NumberValue where
  encodeJson a = case a of
    ACos x -> "acos" := x ~> jsonEmptyObject
    ASin x -> "asin" := x ~> jsonEmptyObject
    ATan x -> "atan" := x ~> jsonEmptyObject
    ATan2 x y -> "atan2" := (Tuple x y) ~> jsonEmptyObject
    Cos x -> "cos" := x ~> jsonEmptyObject
    Sin x -> "sin" := x ~> jsonEmptyObject
    Tan x -> "tan" := x ~> jsonEmptyObject
    Ceil x -> "ceil" := x ~> jsonEmptyObject
    Floor x -> "floor" := x ~> jsonEmptyObject
    Round x -> "round" := x ~> jsonEmptyObject
    Trunc x -> "trunc" := x ~> jsonEmptyObject
    Exp x -> "exp" := x ~> jsonEmptyObject
    Log x -> "log" := x ~> jsonEmptyObject
    Pow x y -> "pow" := (Tuple x y) ~> jsonEmptyObject
    Sqrt x -> "sqrt" := x ~> jsonEmptyObject
    Remainder x y -> "rem" := (Tuple x y) ~> jsonEmptyObject
    Value x -> "equ" := x ~> jsonEmptyObject
    Constant x -> "con" := x ~> jsonEmptyObject
instance decodeJsonNumberValue :: DecodeJson NumberValue where
  decodeJson json = do
    o <- decodeJson json
    let acos' = ACos <$> o .: "acos"
        asin' = ASin <$> o .: "asin"
        atan' = ATan <$> o .: "atan"
        atan2' = (\(Tuple x y) -> ATan2 x y) <$> o .: "atan2"
        cos' = Cos <$> o .: "cos"
        sin' = Sin <$> o .: "sin"
        tan' = Tan <$> o .: "tan"
        ceil' = Ceil <$> o .: "ceil"
        floor' = Floor <$> o .: "floor"
        round' = Round <$> o .: "round"
        trunc' = Trunc <$> o .: "trunc"
        exp' = Exp <$> o .: "exp"
        log' = Log <$> o .: "log"
        pow' = (\(Tuple x y) -> Pow x y) <$> o .: "pow"
        sqrt' = Sqrt <$> o .: "sqrt"
        remainder' = (\(Tuple x y) -> Remainder x y) <$> o .: "rem"
        value' = Value <$> o .: "equ"
        constant' = Constant <$> o .: "con"
    acos'
      <|> asin'
      <|> atan'
      <|> atan2'
      <|> cos'
      <|> sin'
      <|> tan'
      <|> ceil'
      <|> floor'
      <|> round'
      <|> trunc'
      <|> exp'
      <|> log'
      <|> pow'
      <|> sqrt'
      <|> remainder'
      <|> value'
      <|> constant'
instance encodeArrayBufferNumberValue :: EncodeArrayBuffer NumberValue where
  putArrayBuffer b o a = do
    let put2 e x y = do
          mW <- putArrayBuffer b o' x
          case mW of
            Nothing -> throw ("Couldn't write first argument to " <> e)
            Just w -> do
              mW' <- putArrayBuffer b (o' + w) y
              case mW' of
                Nothing -> throw ("Couldn't write second argument to " <> e)
                Just w' -> pure (Just (w + w'))
        o' = o + 1
        Tuple val cont = case a of
          ACos x -> Tuple 0 (putArrayBuffer b o' x)
          ASin x -> Tuple 1 (putArrayBuffer b o' x)
          ATan x -> Tuple 2 (putArrayBuffer b o' x)
          ATan2 x y -> Tuple 3 (put2 "ATan2" x y)
          Cos x -> Tuple 4 (putArrayBuffer b o' x)
          Sin x -> Tuple 5 (putArrayBuffer b o' x)
          Tan x -> Tuple 6 (putArrayBuffer b o' x)
          Ceil x -> Tuple 7 (putArrayBuffer b o' x)
          Floor x -> Tuple 8 (putArrayBuffer b o' x)
          Round x -> Tuple 9 (putArrayBuffer b o' x)
          Trunc x -> Tuple 10 (putArrayBuffer b o' x)
          Exp x -> Tuple 11 (putArrayBuffer b o' x)
          Log x -> Tuple 12 (putArrayBuffer b o' x)
          Pow x y -> Tuple 13 (put2 "Pow" x y)
          Sqrt x -> Tuple 14 (putArrayBuffer b o' x)
          Remainder x y -> Tuple 15 (put2 "Remainder" x y)
          Value x -> Tuple 16 (putArrayBuffer b o' (map Float64BE x))
          Constant x -> Tuple 17 (putArrayBuffer b o' x)
    mW <- putArrayBuffer b o (Uint8 (UInt.fromInt val))
    case mW of
      Nothing -> pure Nothing
      Just _ -> (map (_ + 1)) <$> cont
instance dynamicByteLengthNumberValue :: DynamicByteLength NumberValue where
  byteLength a = (_ + 1) <$> case a of
    ACos x -> byteLength x
    ASin x -> byteLength x
    ATan x -> byteLength x
    ATan2 x y -> (+) <$> byteLength x <*> byteLength y
    Cos x -> byteLength x
    Sin x -> byteLength x
    Tan x -> byteLength x
    Ceil x -> byteLength x
    Floor x -> byteLength x
    Round x -> byteLength x
    Trunc x -> byteLength x
    Exp x -> byteLength x
    Log x -> byteLength x
    Pow x y -> (+) <$> byteLength x <*> byteLength y
    Sqrt x -> byteLength x
    Remainder x y -> (+) <$> byteLength x <*> byteLength y
    Value x -> byteLength (map Float64BE x)
    Constant x -> byteLength x
instance decodeArrayBufferNumberValue :: DecodeArrayBuffer NumberValue where
  readArrayBuffer b o = do
    mFlag <- readArrayBuffer b o
    let o' = o + 1
        overMaybe :: forall q w
                   . DecodeArrayBuffer q
                  => String -> ByteLength -> (q -> w) -> Effect (Maybe w)
        overMaybe e l f = do
          mX <- readArrayBuffer b (o' + l)
          case mX of
            Nothing -> throw ("Couldn't read argument to " <> e)
            Just x -> pure (Just (f x))
        overBoth :: forall q w e
                  . DecodeArrayBuffer q
                 => DecodeArrayBuffer w
                 => DynamicByteLength q
                 => String -> (q -> w -> e) -> Effect (Maybe e)
        overBoth e f = do
          mX <- readArrayBuffer b o'
          case mX of
            Nothing -> throw ("Couldn't read first argument to " <> e)
            Just x -> do
              l <- byteLength x
              overMaybe ("second " <> e) l (f x)
    case mFlag of
      Nothing -> pure Nothing
      Just (Uint8 f) -> case UInt.toInt f of
        flag
          | eq flag 0 -> overMaybe "ACos" 0 ACos
          | eq flag 1 -> overMaybe "ASin" 0 ASin
          | eq flag 2 -> overMaybe "ATan" 0 ATan
          | eq flag 3 -> overBoth "ATan2" ATan2
          | eq flag 4 -> overMaybe "Cos" 0 Cos
          | eq flag 5 -> overMaybe "Sin" 0 Sin
          | eq flag 6 -> overMaybe "Tan" 0 Tan
          | eq flag 7 -> overMaybe "Ceil" 0 Ceil
          | eq flag 8 -> overMaybe "Floor" 0 Floor
          | eq flag 9 -> overMaybe "Round" 0 Round
          | eq flag 10 -> overMaybe "Trunc" 0 Trunc
          | eq flag 11 -> overMaybe "Exp" 0 Exp
          | eq flag 12 -> overMaybe "Log" 0 Log
          | eq flag 13 -> overBoth "Pow" Pow
          | eq flag 14 -> overMaybe "Sqrt" 0 Sqrt
          | eq flag 15 -> overBoth "Remainder" Remainder
          | eq flag 16 -> overMaybe "Value" 0 (\x -> Value (map (\(Float64BE y) -> y) x))
          | eq flag 17 -> overMaybe "Constant" 0 Constant
          | otherwise -> throw "Not a Value"
instance arbitraryNumberValue :: Arbitrary NumberValue where
  arbitrary = sized \s -> tailRecM go (Tuple identity s)
    where
      go :: Tuple (NumberValue -> NumberValue) Int
         -> Gen (Step (Tuple (NumberValue -> NumberValue) Int) NumberValue)
      go (Tuple wrap n)
        | n <= 1 = (\c -> Done (wrap (Constant c))) <$> arbitrary
        | otherwise = do
          chosenF <-
            let x = NonEmpty (pure ASin)
                  [ pure ATan
                  , ATan2 <$> (Constant <$> arbitrary)
                  , pure Cos
                  , pure Sin
                  , pure Tan
                  , pure Ceil
                  , pure Floor
                  , pure Round
                  , pure Trunc
                  , pure Exp
                  , pure Log
                  , Pow <$> (Constant <$> arbitrary)
                  , pure Sqrt
                  , Remainder <$> (Constant <$> arbitrary)
                  ]
            in  oneOf x
          pure (Loop (Tuple chosenF (n - 1)))




data Value a
  = Lit a -- ^ Numeric literal
  | Var VarName -- ^ Use a free variable
  | Add (Value a) (Value a)
  | Sub (Value a) (Value a)
  | Negate (Value a)
  | Mul (Value a) (Value a)
  | Div (Value a) (Value a)
  | Recip (Value a) -- ^ Reciporical
  | GCD (Value a) (Value a)
  | LCM (Value a) (Value a)
  | Abs (Value a) -- ^ Absolute Value
  | Max (Value a) (Value a)
  | Min (Value a) (Value a)
  | Modulo (Value a) (Value a)

derive instance genericValue :: Generic (Value a) _
instance eqValue :: Eq a => Eq (Value a) where
  eq a b = case Tuple a b of
    Tuple (Lit x) (Lit y) -> eq x y
    Tuple (Var x) (Var y) -> eq x y
    Tuple (Add x x') (Add y y') -> eq x y && eq x' y'
    Tuple (Sub x x') (Sub y y') -> eq x y && eq x' y'
    Tuple (Negate x) (Negate y) -> eq x y
    Tuple (Mul x x') (Mul y y') -> eq x y && eq x' y'
    Tuple (Div x x') (Div y y') -> eq x y && eq x' y'
    Tuple (Recip x) (Recip y) -> eq x y
    Tuple (GCD x x') (GCD y y') -> eq x y && eq x' y'
    Tuple (LCM x x') (LCM y y') -> eq x y && eq x' y'
    Tuple (Abs x) (Abs y) -> eq x y
    Tuple (Max x x') (Max y y') -> eq x y && eq x' y'
    Tuple (Min x x') (Min y y') -> eq x y && eq x' y'
    Tuple (Modulo x x') (Modulo y y') -> eq x y && eq x' y'
    _ -> false
instance showValue :: Show a => Show (Value a) where
  show a = case a of
    Lit x -> "Lit (" <> show x <> ")"
    Var x -> "Var (" <> show x <> ")"
    Add x y -> "Add (" <> show x <> ") (" <> show y <> ")"
    Sub x y -> "Sub (" <> show x <> ") (" <> show y <> ")"
    Negate x -> "Negate (" <> show x <> ")"
    Mul x y -> "Mul (" <> show x <> ") (" <> show y <> ")"
    Div x y -> "Div (" <> show x <> ") (" <> show y <> ")"
    Recip x -> "Recip (" <> show x <> ")"
    GCD x y -> "GCD (" <> show x <> ") (" <> show y <> ")"
    LCM x y -> "LCM (" <> show x <> ") (" <> show y <> ")"
    Abs x -> "Abs (" <> show x <> ")"
    Max x y -> "Max (" <> show x <> ") (" <> show y <> ")"
    Min x y -> "Min (" <> show x <> ") (" <> show y <> ")"
    Modulo x y -> "Modulo (" <> show x <> ") (" <> show y <> ")"
instance functorValue :: Functor Value where
  map f a = case a of
    Lit x -> Lit (f x)
    Var n -> Var n
    Add x y -> Add (map f x) (map f y)
    Sub x y -> Sub (map f x) (map f y)
    Negate x -> Negate (map f x)
    Mul x y -> Mul (map f x) (map f y)
    Div x y -> Div (map f x) (map f y)
    Recip x -> Recip (map f x)
    GCD x y -> GCD (map f x) (map f y)
    LCM x y -> LCM (map f x) (map f y)
    Abs x -> Abs (map f x)
    Max x y -> Max (map f x) (map f y)
    Min x y -> Min (map f x) (map f y)
    Modulo x y -> Modulo (map f x) (map f y)
instance encodeJsonValue :: EncodeJson a => EncodeJson (Value a) where
  encodeJson a = case a of
    Lit x -> "lit" := x ~> jsonEmptyObject
    Var n -> "var" := n ~> jsonEmptyObject
    Add x y -> "add" := (Tuple x y) ~> jsonEmptyObject
    Sub x y -> "sub" := (Tuple x y) ~> jsonEmptyObject
    Negate x -> "neg" := x ~> jsonEmptyObject
    Mul x y -> "mul" := (Tuple x y) ~> jsonEmptyObject
    Div x y -> "div" := (Tuple x y) ~> jsonEmptyObject
    Recip x -> "rec" := x ~> jsonEmptyObject
    GCD x y -> "gcd" := (Tuple x y) ~> jsonEmptyObject
    LCM x y -> "lcm" := (Tuple x y) ~> jsonEmptyObject
    Abs x -> "abs" := x ~> jsonEmptyObject
    Max x y -> "max" := (Tuple x y) ~> jsonEmptyObject
    Min x y -> "min" := (Tuple x y) ~> jsonEmptyObject
    Modulo x y -> "mod" := (Tuple x y) ~> jsonEmptyObject
instance decodeJsonValue :: DecodeJson a => DecodeJson (Value a) where
  decodeJson json = do
    o <- decodeJson json
    let lit = Lit <$> o .: "lit"
        var = Var <$> o .: "var"
        add = (\(Tuple x y) -> Add x y) <$> o .: "add"
        sub = (\(Tuple x y) -> Sub x y) <$> o .: "sub"
        neg = Negate <$> o .: "neg"
        mul = (\(Tuple x y) -> Mul x y) <$> o .: "mul"
        div' = (\(Tuple x y) -> Div x y) <$> o .: "div"
        recip' = Recip <$> o .: "rec"
        gcd' = (\(Tuple x y) -> GCD x y) <$> o .: "gcd"
        lcm' = (\(Tuple x y) -> LCM x y) <$> o .: "lcm"
        abs' = Abs <$> o .: "abs"
        max' = (\(Tuple x y) -> Max x y) <$> o .: "max"
        min' = (\(Tuple x y) -> Min x y) <$> o .: "min"
        mod' = (\(Tuple x y) -> Modulo x y) <$> o .: "mod"
    lit
      <|> var
      <|> add
      <|> sub
      <|> neg
      <|> mul
      <|> div'
      <|> recip'
      <|> gcd'
      <|> lcm'
      <|> abs'
      <|> max'
      <|> min'
      <|> mod'
instance dynamicByteLengthValue :: DynamicByteLength a => DynamicByteLength (Value a) where
  byteLength a = (_ + 1) <$> case a of
    Lit x -> byteLength x
    Var n -> byteLength n
    Add x y -> (+) <$> byteLength x <*> byteLength y
    Sub x y -> (+) <$> byteLength x <*> byteLength y
    Negate x -> byteLength x
    Mul x y -> (+) <$> byteLength x <*> byteLength y
    Div x y -> (+) <$> byteLength x <*> byteLength y
    Recip x -> byteLength x
    GCD x y -> (+) <$> byteLength x <*> byteLength y
    LCM x y -> (+) <$> byteLength x <*> byteLength y
    Abs x -> byteLength x
    Max x y -> (+) <$> byteLength x <*> byteLength y
    Min x y -> (+) <$> byteLength x <*> byteLength y
    Modulo x y -> (+) <$> byteLength x <*> byteLength y
instance encodeArrayBufferValue :: EncodeArrayBuffer a => EncodeArrayBuffer (Value a) where
  putArrayBuffer b o a = do
    let put2 e x y = do
          mW <- putArrayBuffer b o' x
          case mW of
            Nothing -> throw ("Couldn't write first argument to " <> e)
            Just w -> do
              mW' <- putArrayBuffer b (o' + w) y
              case mW' of
                Nothing -> throw ("Couldn't write second argument to " <> e)
                Just w' -> pure (Just (w + w'))
        o' = o + 1
        Tuple val cont = case a of
          Lit x -> Tuple 0 (putArrayBuffer b o' x)
          Var n -> Tuple 1 (putArrayBuffer b o' n)
          Add x y -> Tuple 2 (put2 "Add" x y)
          Sub x y -> Tuple 3 (put2 "Sub" x y)
          Negate n -> Tuple 4 (putArrayBuffer b o' n)
          Mul x y -> Tuple 5 (put2 "Mul" x y)
          Div x y -> Tuple 6 (put2 "Div" x y)
          Recip n -> Tuple 7 (putArrayBuffer b o' n)
          GCD x y -> Tuple 8 (put2 "GCD" x y)
          LCM x y -> Tuple 9 (put2 "LCM" x y)
          Abs n -> Tuple 10 (putArrayBuffer b o' n)
          Max x y -> Tuple 11 (put2 "Max" x y)
          Min x y -> Tuple 12 (put2 "Min" x y)
          Modulo x y -> Tuple 13 (put2 "Modulo" x y)
    mW <- putArrayBuffer b o (Uint8 (UInt.fromInt val))
    case mW of
      Nothing -> pure Nothing
      Just _ -> (map (_ + 1)) <$> cont
instance decodeArrayBufferValue :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (Value a) where
  readArrayBuffer b o = do
    mFlag <- readArrayBuffer b o
    let o' = o + 1
        overMaybe :: forall q w
                   . DecodeArrayBuffer q
                  => String -> ByteLength -> (q -> w) -> Effect (Maybe w)
        overMaybe e l f = do
          mX <- readArrayBuffer b (o' + l)
          case mX of
            Nothing -> throw ("Couldn't read argument to " <> e)
            Just x -> pure (Just (f x))
        overBoth :: forall q w e
                  . DecodeArrayBuffer q
                 => DecodeArrayBuffer w
                 => DynamicByteLength q
                 => String -> (q -> w -> e) -> Effect (Maybe e)
        overBoth e f = do
          mX <- readArrayBuffer b o'
          case mX of
            Nothing -> throw ("Couldn't read first argument to " <> e)
            Just x -> do
              l <- byteLength x
              overMaybe ("second " <> e) l (f x)
    case mFlag of
      Nothing -> pure Nothing
      Just (Uint8 f) -> case UInt.toInt f of
        flag
          | eq flag 0 -> overMaybe "Lit" 0 Lit
          | eq flag 1 -> overMaybe "Var" 0 Var
          | eq flag 2 -> overBoth "Add" Add
          | eq flag 3 -> overBoth "Sub" Sub
          | eq flag 4 -> overMaybe "Negate" 0 Negate
          | eq flag 5 -> overBoth "Mul" Mul
          | eq flag 6 -> overBoth "Div" Div
          | eq flag 7 -> overMaybe "Recip" 0 Recip
          | eq flag 8 -> overBoth "GCD" GCD
          | eq flag 9 -> overBoth "LCM" LCM
          | eq flag 10 -> overMaybe "Abs" 0 Abs
          | eq flag 11 -> overBoth "Max" Max
          | eq flag 12 -> overBoth "Min" Min
          | eq flag 13 -> overBoth "Modulo" Modulo
          | otherwise -> throw "Not a Value"
instance arbitraryValue :: Arbitrary a => Arbitrary (Value a) where
  arbitrary = genValue arbitrary

type Equation a = Tuple a a



genValue :: forall a. Gen a -> Gen (Value a)
genValue gen = sized \s -> tailRecM go (Tuple identity s)
    where
      small :: Gen (Value a)
      small =
        let q = NonEmpty (Lit <$> gen) [Var <$> arbitrary]
        in  oneOf q
      go :: Tuple (Value a -> Value a) Int
         -> Gen (Step (Tuple (Value a -> Value a) Int) (Value a))
      go (Tuple wrap n)
        | n <= 1 = (\c -> Done (wrap c)) <$> small
        | otherwise = do
          chosenF <-
            let x = NonEmpty (Add <$> small)
                  [ Sub <$> small
                  , pure Negate
                  , Mul <$> small
                  , Div <$> small
                  , pure Recip
                  , GCD <$> small
                  , LCM <$> small
                  , pure Abs
                  , Max <$> small
                  , Min <$> small
                  , Modulo <$> small
                  ]
            in  oneOf x
          pure (Loop (Tuple chosenF (n - 1)))




newtype BoundVars a = BoundVars (Object a)

emptyVars :: forall a. BoundVars a
emptyVars = bindVars []

bindVars :: forall f a. Foldable f => f (Tuple VarName a) -> BoundVars a
bindVars xs = BoundVars (O.fromFoldable xs)

bindVar :: forall a. BoundVars a -> VarName -> a -> BoundVars a
bindVar (BoundVars xs) n v = BoundVars (O.insert n v xs)


data BoundError
  = UnboundVariable VarName
instance showBoundError :: Show BoundError where
  show x = case x of
    UnboundVariable n -> "UnboundVariable" <> show n



computeConstant :: NumberConstant -> Number
computeConstant x = case x of
  E -> e
  Ln2 -> ln2
  Ln10 -> ln10
  Log2E -> log2e
  Log10E -> log10e
  Pi -> pi
  Tau -> tau
  Sqrt1_2 -> sqrt1_2
  Sqrt2 -> sqrt2


computeNumberValue :: NumberValue -> BoundVars Number -> Either BoundError Number
computeNumberValue v nss@(BoundVars ns) = case v of
  ACos x -> acos <$> computeNumberValue x nss
  ASin x -> asin <$> computeNumberValue x nss
  ATan x -> atan <$> computeNumberValue x nss
  ATan2 x y -> atan2 <$> computeNumberValue x nss <*> computeNumberValue y nss
  Cos x -> cos <$> computeNumberValue x nss
  Sin x -> sin <$> computeNumberValue x nss
  Tan x -> tan <$> computeNumberValue x nss
  Ceil x -> ceil <$> computeNumberValue x nss
  Floor x -> floor <$> computeNumberValue x nss
  Round x -> round <$> computeNumberValue x nss
  Trunc x -> trunc <$> computeNumberValue x nss
  Exp x -> exp <$> computeNumberValue x nss
  Log x -> log <$> computeNumberValue x nss
  Pow x y -> pow <$> computeNumberValue x nss <*> computeNumberValue y nss
  Sqrt x -> sqrt <$> computeNumberValue x nss
  Remainder x y -> remainder <$> computeNumberValue x nss <*> computeNumberValue y nss
  Value v' -> computeValue v' nss
  Constant x -> Right (computeConstant x)


computeValue :: forall a
              . EuclideanRing a
             => DivisionRing a
             => Eq a
             => Ord a
             => Value a -> BoundVars a -> Either BoundError a
computeValue eq nss@(BoundVars ns) = case eq of
  Lit v -> Right v
  Var n -> case O.lookup n ns of
    Nothing -> Left (UnboundVariable n)
    Just v -> Right v
  Add x y -> (+) <$> computeValue x nss <*> computeValue y nss
  Sub x y -> (-) <$> computeValue x nss <*> computeValue y nss
  Negate x -> negate <$> computeValue x nss
  Mul x y -> (*) <$> computeValue x nss <*> computeValue y nss
  Div x y -> (/) <$> computeValue x nss <*> computeValue y nss
  Recip x -> recip <$> computeValue x nss
  GCD x y -> gcd <$> computeValue x nss <*> computeValue y nss
  LCM x y -> lcm <$> computeValue x nss <*> computeValue y nss
  Abs x -> abs <$> computeValue x nss
  Max x y -> max <$> computeValue x nss <*> computeValue y nss
  Min x y -> min <$> computeValue x nss <*> computeValue y nss
  Modulo x y -> mod <$> computeValue x nss <*> computeValue y nss
