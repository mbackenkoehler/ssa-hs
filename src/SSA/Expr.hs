{-|
  Module      : SSA.Expr
  Description : Expression representation and evaluation
  Copyright   : 2016 Michael Backenköhler
  License     : MIT
  Maintainer  : Michael Backenköhler
  Stability   : experimental
  Portability : portable
-}
------------------------------------------------------------------------------
module SSA.Expr
      ( Expr(..), Atom(..), BinOperator(..)
      , subs, simplify, vectorize, op, anyExpr
      ) where
------------------------------------------------------------------------------
import           Data.List ( group, all )
import           Data.Maybe
------------------------------------------------------------------------------
data Expr
  = Atomic !Atom
  | BinOp !BinOperator !Expr !Expr
  deriving (Eq, Show)

data Atom
  = Const {-# UNPACK #-}!Double
  | Var !String
  | Id {-# UNPACK #-}!Int
  deriving (Eq, Show)

data BinOperator = Add | Sub | Mul | Div
  deriving (Eq, Show)
------------------------------------------------------------------------------
unique :: Eq a => [a] -> Bool
unique = all ((==1) . length) . group

simplify :: Expr -> Expr
simplify (BinOp t e1 e2) = tryEval $ BinOp t (tryEval e1) (tryEval e2)
  where
    tryEval e = fromMaybe e (Atomic . Const <$> eval e)
simplify e = e

eval :: Expr -> Maybe Double
eval (Atomic (Const c)) = Just c
eval (Atomic _) = Nothing
eval (BinOp f e1 e2) = op f <$> eval e1 <*> eval e2

op :: BinOperator -> Double -> Double -> Double
op Add = (+)
op Sub = (-)
op Mul = (*)
op Div = (/)

subs :: [(String, Double)] -> Expr -> Expr
subs cs e@(Atomic (Var id)) = fromMaybe e (Atomic . Const <$> lookup id cs)
subs cs (BinOp t e1 e2) = BinOp t (subs cs e1) (subs cs e2)
subs _ e = e

-- | Substitute species variables by their index. Assumes that all
-- other variables have been substituted
vectorize :: [String] -> Expr -> Expr
vectorize s (Atomic (Var name)) = Atomic $ Id $ length $ takeWhile (/= name) s
vectorize s (BinOp f e1 e2) = BinOp f (vectorize s e1) (vectorize s e2)
vectorize s e = e

anyExpr :: (Expr -> Bool) -> Expr -> Bool
anyExpr f b@(BinOp _ e1 e2) = f b || anyExpr f e1 || anyExpr f e2
anyExpr f e = f e
