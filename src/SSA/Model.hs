{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
{-|
  Module      : SSA.Model
  Description : SSA model representation
  Copyright   : 2016 Michael Backenköhler
  License     : MIT
  Maintainer  : Michael Backenköhler
  Stability   : experimental
  Portability : portable
-}
------------------------------------------------------------------------------
module SSA.Model
      ( State, Ident, Nat, Species(..), SpeciesType(..), PModel(..)
      , PReaction(..), Educt, Product, Name, Model(..), Rate, Reaction(..)
      , transformModel
      , applyChange
      ) where
------------------------------------------------------------------------------
import           Data.Maybe ( fromMaybe )
import qualified Data.Text   as T
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed ( (!) )
------------------------------------------------------------------------------
import           SSA.Expr
------------------------------------------------------------------------------
type State = V.Vector Int

data PModel = PModel
  { parameters :: [(Ident, Double)]
  , species    :: [Species]
  , reacts     :: [PReaction]
  , initials   :: [(Ident, Nat)]
  } deriving Show

type Ident = String

type Nat = Int

data Species = Species SpeciesType Ident
  deriving (Show)

data SpeciesType = LCopy | HCopy
  deriving (Eq, Show)

data PReaction = PReaction Educt Product Expr
  deriving (Show)

type Educt = [(Ident, Nat)]

type Product = [(Ident, Nat)]

type Name = T.Text

data Model = Model
  { names     :: [Name]
  , reactions :: [Reaction]
  , initial   :: State
  }

type Rate = Double

data Reaction = Reaction
  { propensity :: State -> Rate
  , change     :: State
  }
------------------------------------------------------------------------------
evalV :: Expr -> State -> Double
evalV (Atomic (Const c)) _ = c
evalV (Atomic (Id i)) s = fromIntegral $ s ! i
evalV (BinOp o e1 e2) s = op o (evalV e1 s) (evalV e2 s)
evalV e _ = error $ "evalV: unexpected expression: " ++ show e

processReactions :: PModel -> [Reaction]
processReactions m@PModel{..} = fmap react reacts
  where
    names' = processNames m
    react (PReaction i o p) = Reaction { propensity = prop p
                                       , change = chng (vecRs i) (vecRs o) }
    prop = evalV . vectorize names' . simplify . subs parameters
    vecRs v = [ fromMaybe 0 (lookup s v) | s <- names' ]
    chng i o = V.fromList (zipWith (flip (-)) i o)

processNames :: PModel -> [String]
processNames = fmap (\(Species _ name) -> name) . species

applyChange :: State -> State -> State
applyChange = V.zipWith (+)

transformModel :: PModel -> Model
transformModel m@PModel{..} = Model
  { names = T.pack <$> processNames m
  , reactions = processReactions m
  , initial = V.fromList [ fromMaybe 0 (lookup s initials) | s <- processNames m ]
  }
