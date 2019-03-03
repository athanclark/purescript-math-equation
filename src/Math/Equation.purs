-- | This module defines data structures that mirror common Math functions, to
-- | ease the storage of such an equation.

module Math.Equation where

import Prelude (class Eq, class Ord, pure, (<$>), (<*>), (+), (-), (*), (/), negate, recip, gcd, lcm, min, max, mod)
import Data.Ord (abs)
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple)
import Data.Either (Either (Left))
import Data.Foldable (class Foldable)
import Data.EuclideanRing (class EuclideanRing)
import Data.DivisionRing (class DivisionRing)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable, insert, lookup) as O
import Math (acos, asin, atan, atan2, cos, sin, tan, ceil, floor, round, trunc, exp, log, pow, sqrt, remainder, e, pi, tau, ln2, ln10, log2e, log10e, sqrt1_2, sqrt2)


type VarName = String


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
  | E -- ^ `e`, Euler's constant
  | Ln2 -- ^ `log_e(2)`
  | Ln10 -- ^ `log_e(10)`
  | Log2E -- ^ `log_2(e)`
  | Log10E -- ^ `log_10(e)`
  | Pi
  | Tau
  | Sqrt1_2 -- ^ Square-root of `0.5`
  | Sqrt2 -- ^ Square-root of `2`
  | Equation (Equation Number)


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


newtype BoundVars a = BoundVars (Object a)

emptyVars :: forall a. BoundVars a
emptyVars = bindVars []

bindVars :: forall f a. Foldable f => f (Tuple VarName a) -> BoundVars a
bindVars xs = BoundVars (O.fromFoldable xs)

bindVar :: forall a. BoundVars a -> VarName -> a -> BoundVars a
bindVar (BoundVars xs) n v = BoundVars (O.insert n v xs)


data BoundError
  = UnboundVariable VarName


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
  E -> pure e
  Ln2 -> pure ln2
  Ln10 -> pure ln10
  Log2E -> pure log2e
  Log10E -> pure log10e
  Pi -> pure pi
  Tau -> pure tau
  Sqrt1_2 -> pure sqrt1_2
  Sqrt2 -> pure sqrt2
  Equation eq' -> compute eq' nss


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
