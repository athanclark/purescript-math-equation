-- | This module defines data structures that mirror common Math functions, to
-- | ease the storage of such an equation.

module Math.Equation where

import Prelude (class Eq, eq, (&&), class Show, class Ord, pure, (<$>), (<*>), (+), (-), (*), (/), negate, recip, gcd, lcm, min, max, mod)
import Data.Ord (abs)
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (Left))
import Data.Foldable (class Foldable)
import Data.EuclideanRing (class EuclideanRing)
import Data.DivisionRing (class DivisionRing)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable, insert, lookup) as O
import Math (acos, asin, atan, atan2, cos, sin, tan, ceil, floor, round, trunc, exp, log, pow, sqrt, remainder, e, pi, tau, ln2, ln10, log2e, log10e, sqrt1_2, sqrt2)


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
-- instance eqNumberEquation :: Eq NumberEquation where
--   eq = genericEq
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
  eq = genericEq
instance showEquation :: Show a => Show (Equation a) where
  show = genericShow


newtype BoundVars a = BoundVars (Object a)

emptyVars :: forall a. BoundVars a
emptyVars = bindVars []

bindVars :: forall f a. Foldable f => f (Tuple VarName a) -> BoundVars a
bindVars xs = BoundVars (O.fromFoldable xs)

bindVar :: forall a. BoundVars a -> VarName -> a -> BoundVars a
bindVar (BoundVars xs) n v = BoundVars (O.insert n v xs)


data BoundError
  = UnboundVariable VarName


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
  Constant x -> pure (computeConstant x)


compute :: forall a
         . EuclideanRing a
        => DivisionRing a
        => Eq a
        => Ord a
        => Equation a -> BoundVars a -> Either BoundError a
compute eq nss@(BoundVars ns) = case eq of
  Lit v -> pure v
  Var n -> case O.lookup n ns of
    Nothing -> Left (UnboundVariable n)
    Just v -> pure v
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
