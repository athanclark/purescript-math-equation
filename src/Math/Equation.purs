-- | This module defines data structures that mirror common Math functions, to
-- | ease the storage of such an equation.

module Math.Equation where

import Prelude (pure, (<$>), (<*>), (+), (-), (*), (/), negate, recip, gcd, lcm, min, max, mod)
import Data.Ord (abs)
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple)
import Data.Either (Either (Left))
import Data.Foldable (class Foldable)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable, insert, lookup) as O
import Math (acos, asin, atan, atan2, cos, sin, tan, ceil, floor, round, trunc, exp, log, pow, sqrt, remainder, e, pi, tau, ln2, ln10, log2e, log10e, sqrt1_2, sqrt2)


type VarName = String


data Equation
  = Lit Number -- ^ Numeric literal
  | Var VarName -- ^ Use a free variable
  | Add Equation Equation
  | Sub Equation Equation
  | Negate Equation
  | Mul Equation Equation
  | Div Equation Equation
  | Recip Equation -- ^ Reciporical
  | GCD Equation Equation
  | LCM Equation Equation
  | Abs Equation -- ^ Absolute Value
  | ACos Equation
  | ASin Equation
  | ATan Equation
  | ATan2 Equation Equation
  | Cos Equation
  | Sin Equation
  | Tan Equation
  | Ceil Equation -- ^ Round-up
  | Floor Equation -- ^ Round-down
  | Round Equation -- ^ Round-middle
  | Trunc Equation -- ^ `if x > 0 then floor(x) else ceil(x)`
  | Exp Equation -- ^ `e^x`
  | Log Equation -- ^ `log_e(x)`, natural logarithm
  | Pow Equation Equation -- ^ `x^y`
  | Sqrt Equation
  | Max Equation Equation
  | Min Equation Equation
  | Remainder Equation Equation
  | Modulo Equation Equation
  | E -- ^ `e`, Euler's constant
  | Ln2 -- ^ `log_e(2)`
  | Ln10 -- ^ `log_e(10)`
  | Log2E -- ^ `log_2(e)`
  | Log10E -- ^ `log_10(e)`
  | Pi
  | Tau
  | Sqrt1_2 -- ^ Square-root of `0.5`
  | Sqrt2 -- ^ Square-root of `2`


newtype BoundVars = BoundVars (Object Number)

emptyVars :: BoundVars
emptyVars = bindVars []

bindVars :: forall f. Foldable f => f (Tuple VarName Number) -> BoundVars
bindVars xs = BoundVars (O.fromFoldable xs)

bindVar :: BoundVars -> VarName -> Number -> BoundVars
bindVar (BoundVars xs) n v = BoundVars (O.insert n v xs)


data BoundError
  = UnboundVariable VarName


compute :: Equation -> BoundVars -> Either BoundError Number
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
  ACos x -> acos <$> compute x nss
  ASin x -> asin <$> compute x nss
  ATan x -> atan <$> compute x nss
  ATan2 x y -> atan2 <$> compute x nss <*> compute y nss
  Cos x -> cos <$> compute x nss
  Sin x -> sin <$> compute x nss
  Tan x -> tan <$> compute x nss
  Ceil x -> ceil <$> compute x nss
  Floor x -> floor <$> compute x nss
  Round x -> round <$> compute x nss
  Trunc x -> trunc <$> compute x nss
  Exp x -> exp <$> compute x nss
  Log x -> log <$> compute x nss
  Max x y -> max <$> compute x nss <*> compute y nss
  Min x y -> min <$> compute x nss <*> compute y nss
  Pow x y -> pow <$> compute x nss <*> compute y nss
  Sqrt x -> sqrt <$> compute x nss
  Remainder x y -> remainder <$> compute x nss <*> compute y nss
  Modulo x y -> mod <$> compute x nss <*> compute y nss
  E -> pure e
  Ln2 -> pure ln2
  Ln10 -> pure ln10
  Log2E -> pure log2e
  Log10E -> pure log10e
  Pi -> pure pi
  Tau -> pure tau
  Sqrt1_2 -> pure sqrt1_2
  Sqrt2 -> pure sqrt2
