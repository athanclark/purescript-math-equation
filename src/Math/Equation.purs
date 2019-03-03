-- | This module defines data structures that mirror common Math functions, to
-- | ease the storage of such an equation.

module Math.Equation
  ( VarName, BoundVars, emptyVars, bindVars, bindVar, BoundError (..)
  , NumberConstant (..), NumberEquation (..), Equation (..)
  , computeConstant, computeNumber, compute
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
import Control.Alternative ((<|>))
import Control.Monad.Rec.Class (tailRecM, Step (..))
import Foreign.Object (Object)
import Foreign.Object (fromFoldable, insert, lookup) as O
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
instance arbitraryNumberConstant :: Arbitrary NumberConstant where
  arbitrary = genericArbitrary
instance coarbitraryNumberConstant :: Coarbitrary NumberConstant where
  coarbitrary x = coarbitrary (from x) -- genericCoarbitrary


data NumberEquation
  = ACos NumberEquation
  | ASin NumberEquation
  | ATan NumberEquation
  | ATan2 NumberEquation NumberEquation
  | Cos NumberEquation
  | Sin NumberEquation
  | Tan NumberEquation
  | Ceil NumberEquation -- ^ Round-up
  | Floor NumberEquation -- ^ Round-down
  | Round NumberEquation -- ^ Round-middle
  | Trunc NumberEquation -- ^ `if x > 0 then floor(x) else ceil(x)`
  | Exp NumberEquation -- ^ `e^x`
  | Log NumberEquation -- ^ `log_e(x)`, natural logarithm
  | Pow NumberEquation NumberEquation -- ^ `x^y`
  | Sqrt NumberEquation
  | Remainder NumberEquation NumberEquation
  | Equation (Equation Number)
  | Constant NumberConstant

derive instance genericNumberEquation :: Generic NumberEquation _
instance eqNumberEquation :: Eq NumberEquation where
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
    Tuple (Equation x) (Equation y) -> eq x y
    Tuple (Constant x) (Constant y) -> eq x y
    _ -> false
instance showNumberEquation :: Show NumberEquation where
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
    Equation x -> "Equation (" <> show x <> ")"
    Constant x -> "Constant (" <> show x <> ")"
instance encodeJsonNumberEquation :: EncodeJson NumberEquation where
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
    Equation x -> "equ" := x ~> jsonEmptyObject
    Constant x -> "con" := x ~> jsonEmptyObject
instance decodeJsonNumberEquation :: DecodeJson NumberEquation where
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
        equation' = Equation <$> o .: "equ"
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
      <|> equation'
      <|> constant'
instance arbitraryNumberEquation :: Arbitrary NumberEquation where
  arbitrary = sized \s -> tailRecM go (Tuple identity s)
    where
      go :: Tuple (NumberEquation -> NumberEquation) Int
         -> Gen (Step (Tuple (NumberEquation -> NumberEquation) Int) NumberEquation)
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
                  -- , pure Equation
                  ]
            in  oneOf x
          pure (Loop (Tuple chosenF (n - 1)))
-- instance coarbitraryEquation :: Coarbitrary a => Coarbitrary (Equation a) where
--   coarbitrary x = coarbitrary (from x) -- genericCoarbitrary




data Equation a
  = Lit a -- ^ Numeric literal
  | Var VarName -- ^ Use a free variable
  | Add (Equation a) (Equation a)
  | Sub (Equation a) (Equation a)
  | Negate (Equation a)
  | Mul (Equation a) (Equation a)
  | Div (Equation a) (Equation a)
  | Recip (Equation a) -- ^ Reciporical
  | GCD (Equation a) (Equation a)
  | LCM (Equation a) (Equation a)
  | Abs (Equation a) -- ^ Absolute Value
  | Max (Equation a) (Equation a)
  | Min (Equation a) (Equation a)
  | Modulo (Equation a) (Equation a)

derive instance genericEquation :: Generic (Equation a) _
instance eqEquation :: Eq a => Eq (Equation a) where
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
instance showEquation :: Show a => Show (Equation a) where
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
instance functorEquation :: Functor Equation where
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
instance encodeJsonEquation :: EncodeJson a => EncodeJson (Equation a) where
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
instance decodeJsonEquation :: DecodeJson a => DecodeJson (Equation a) where
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
instance arbitraryEquation :: Arbitrary a => Arbitrary (Equation a) where
  arbitrary = sized \s -> tailRecM go (Tuple identity s)
    where
      small :: Gen (Equation a)
      small =
        let q = NonEmpty (Lit <$> arbitrary) [Var <$> arbitrary]
        in  oneOf q
      go :: Tuple (Equation a -> Equation a) Int
         -> Gen (Step (Tuple (Equation a -> Equation a) Int) (Equation a))
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


computeNumber :: NumberEquation -> BoundVars Number -> Either BoundError Number
computeNumber eq nss@(BoundVars ns) = case eq of
  ACos x -> acos <$> computeNumber x nss
  ASin x -> asin <$> computeNumber x nss
  ATan x -> atan <$> computeNumber x nss
  ATan2 x y -> atan2 <$> computeNumber x nss <*> computeNumber y nss
  Cos x -> cos <$> computeNumber x nss
  Sin x -> sin <$> computeNumber x nss
  Tan x -> tan <$> computeNumber x nss
  Ceil x -> ceil <$> computeNumber x nss
  Floor x -> floor <$> computeNumber x nss
  Round x -> round <$> computeNumber x nss
  Trunc x -> trunc <$> computeNumber x nss
  Exp x -> exp <$> computeNumber x nss
  Log x -> log <$> computeNumber x nss
  Pow x y -> pow <$> computeNumber x nss <*> computeNumber y nss
  Sqrt x -> sqrt <$> computeNumber x nss
  Remainder x y -> remainder <$> computeNumber x nss <*> computeNumber y nss
  Equation eq' -> compute eq' nss
  Constant x -> Right (computeConstant x)


compute :: forall a
         . EuclideanRing a
        => DivisionRing a
        => Eq a
        => Ord a
        => Equation a -> BoundVars a -> Either BoundError a
compute eq nss@(BoundVars ns) = case eq of
  Lit v -> Right v
  Var n -> case O.lookup n ns of
    Nothing -> Left (UnboundVariable n)
    Just v -> Right v
  Add x y -> (+) <$> compute x nss <*> compute y nss
  Sub x y -> (-) <$> compute x nss <*> compute y nss
  Negate x -> negate <$> compute x nss
  Mul x y -> (*) <$> compute x nss <*> compute y nss
  Div x y -> (/) <$> compute x nss <*> compute y nss
  Recip x -> recip <$> compute x nss
  GCD x y -> gcd <$> compute x nss <*> compute y nss
  LCM x y -> lcm <$> compute x nss <*> compute y nss
  Abs x -> abs <$> compute x nss
  Max x y -> max <$> compute x nss <*> compute y nss
  Min x y -> min <$> compute x nss <*> compute y nss
  Modulo x y -> mod <$> compute x nss <*> compute y nss
