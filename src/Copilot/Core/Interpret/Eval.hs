--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An tagless interpreter for Copilot specifications.

module Copilot.Core.Interpret.Eval
  ( Env
  , Output
  , ExecTrace (..)
  , eval
  ) where

import Copilot.Core
import Copilot.Core.Type.Dynamic
import Copilot.Core.Type.Show (showWithType)
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (id)
import qualified Prelude as P

--------------------------------------------------------------------------------

type Env k = [(k, DynamicF [] Type)]

--------------------------------------------------------------------------------

type Output = String

data ExecTrace = ExecTrace
  { interpTriggers  :: Map String [Maybe [Output]]
  , interpObservers :: Map String [Output] }
  deriving Show

--------------------------------------------------------------------------------

eval :: Int -> Env Name -> Spec -> ExecTrace
eval k exts spec =
  let
    strms = fmap (evalStream   k exts strms) (specStreams   spec)
    trigs = fmap (evalTrigger  k exts strms) (specTriggers  spec)
    obsvs = fmap (evalObserver k exts strms) (specObservers spec)
  in
    ExecTrace
      { interpTriggers  = M.fromList $ 
          zip (fmap triggerName  (specTriggers  spec)) trigs
      , interpObservers = M.fromList $
          zip (fmap observerName (specObservers spec)) obsvs
      }

--------------------------------------------------------------------------------

strictList :: [a] -> [a]
strictList []     = []
strictList (x:xs) = x `seq` (x : strictList xs)

strictEval :: EvalExpr α -> EvalExpr α
strictEval e = e `seq` EvalExpr $
  \ exts locs strms -> strictList (evalExpr_ e exts locs strms)

--------------------------------------------------------------------------------

class Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

--------------------------------------------------------------------------------

newtype EvalExpr a = EvalExpr
  { evalExpr_ :: Env Name -> Env Name -> Env Id -> [a] }

instance Applicative EvalExpr where
  pure x    = EvalExpr $ \ _ _ _ -> repeat x
  e1 <*> e2 = EvalExpr $ \ exts locs strms -> e1 `seq` e2 `seq`
    zipWith ($) (evalExpr_ e1 exts locs strms) (evalExpr_ e2 exts locs strms)

instance Expr EvalExpr where
  const _ x              = x `seq` pure x
  drop t i id            = EvalExpr $ \ _ _ strms -> strictList $
                             let
                               Just xs = lookup id strms >>= fromDynamicF t
                             in
                               P.drop (fromIntegral i) xs
  local t1 _  name e1 e2 = EvalExpr $ \ exts locs strms -> strictList $
                             let
                               xs    = evalExpr_ e1 exts locs strms
                               locs' = (name, toDynamicF xs t1) : locs
                             in
                               evalExpr_ e2 exts locs' strms
  var t name             = EvalExpr $ \ _ locs _ -> strictList $
                             let
                               Just xs = lookup name locs >>= fromDynamicF t
                             in
                               xs
  extern t name          = EvalExpr $ \ exts _ _ -> strictList $
                             evalExtern t name exts
  op1 op e1              = strictEval $ pure op <*> e1
  op2 op e1 e2           = strictEval $ pure (apply2 op) <*> e1 <*> e2
  op3 op e1 e2 e3        = strictEval $ pure (apply3 op) <*> e1 <*> e2 <*> e3

evalExtern :: Type a -> Name -> Env Name -> [a]
evalExtern t name exts =
  case lookup name exts of
    Nothing -> error $ "Undefined external variable: " ++ name
    Just dyn ->
      case fromDynamicF t dyn of
        Nothing -> error $ "Ill-typed external variable: " ++ name
        Just xs -> xs

--------------------------------------------------------------------------------

instance Op1 (->) where
  not      = P.not
  abs _    = P.abs
  sign _   = P.signum
  recip _  = P.recip
  exp _    = P.exp
  sqrt _   = P.sqrt
  log _    = P.log
  sin _    = P.sin
  tan _    = P.tan
  cos _    = P.cos
  asin _   = P.asin
  atan _   = P.atan
  acos _   = P.acos
  sinh _   = P.sinh
  tanh _   = P.tanh
  cosh _   = P.cosh
  asinh _  = P.asinh
  atanh _  = P.atanh
  acosh _  = P.acosh

--------------------------------------------------------------------------------

newtype Apply2 a b c = Apply2 { apply2 :: a -> b -> c }

instance Op2 Apply2 where
  and      = Apply2 (&&)
  or       = Apply2 (||)
  add _    = Apply2 (+)
  sub _    = Apply2 (-)
  mul _    = Apply2 (*)
  mod _    = Apply2 (catchZero P.mod)
  div _    = Apply2 (catchZero P.div)
  fdiv _   = Apply2 (P./)
  pow _    = Apply2 (P.**)
  logb _   = Apply2 P.logBase
  eq _     = Apply2 (==)
  ne _     = Apply2 (/=)
  le _     = Apply2 (<=)
  ge _     = Apply2 (>=)
  lt _     = Apply2 (<)
  gt _     = Apply2 (>)

catchZero :: Integral a => (a -> a -> a) -> (a -> a -> a)
catchZero _ _ 0 = 0
catchZero f x y = f x y

--------------------------------------------------------------------------------

newtype Apply3 a b c d = Apply3 { apply3 :: a -> b -> c -> d }

instance Op3 Apply3 where
  mux _    = Apply3 $ \ v x y -> if v then x else y

--------------------------------------------------------------------------------

evalStream :: Int -> Env Name -> Env Id -> Stream -> (Int, DynamicF [] Type)
evalStream k exts strms
  Stream
    { streamId       = id
    , streamBuffer   = buffer
    , streamExpr     = e
    , streamExprType = t
    } = (id, toDynamicF xs t)

  where

  xs = take k $ strictList $ buffer ++ evalExpr_ e exts [] strms
--  ys =
--    case mguard of
--      Just e2 -> withGuard (uninitialized t) (evalExpr env e2) xs
--      Nothing -> xs

--  withGuard :: a -> [Bool] -> [a] -> [a]
--  withGuard _ (True:vs)  (x:xs) = x : withGuard x vs xs
--  withGuard z (False:vs) xs     = z : withGuard z vs xs
--  withGuard _ _          _      = error "withGuard: empty stream."

--------------------------------------------------------------------------------

evalTrigger :: Int -> Env Name -> Env Id -> Trigger -> [Maybe [Output]]
evalTrigger k exts strms
  Trigger
    { triggerGuard = e
    , triggerArgs  = args
    } = take k $ map tag (zip bs vs)

  where

  tag :: (Bool, a) -> Maybe a
  tag (True,  x) = Just x
  tag (False, _) = Nothing

  bs :: [Bool]
  bs = evalExpr_ e exts [] strms

  vs :: [[Output]]
  vs = transpose $ map evalTriggerArg args

  evalTriggerArg :: TriggerArg -> [Output]
  evalTriggerArg (TriggerArg e1 t) =
    map (showWithType t) (evalExpr_ e1 exts [] strms)

--------------------------------------------------------------------------------


evalObserver :: Int -> Env Name -> Env Id -> Observer -> [Output]
evalObserver k exts strms
  Observer
    { observerExpr     = e
    , observerExprType = t }
  = take k $ strictList $ map (showWithType t) (evalExpr_ e exts [] strms)

--------------------------------------------------------------------------------