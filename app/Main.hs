{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
------------------------------------------------------------------------------
{-|
  Module      : Main
  Description : Program entry point
  Copyright   : 2016 Michael Backenköhler
  License     : MIT
  Maintainer  : Michael Backenköhler
  Stability   : experimental
  Portability : portable
-}
------------------------------------------------------------------------------
module Main ( main ) where
------------------------------------------------------------------------------
import           Control.Monad ( void )
import           Control.Monad.Trans.RWS.Strict ( execRWST )
import           Data.Maybe
import           GHC.Generics
import           Options.Generic
------------------------------------------------------------------------------
import qualified SSA.Parser as SSA
import qualified SSA.Model  as SSA
import qualified SSA.SSA    as SSA
------------------------------------------------------------------------------
data Args = Args
  { model       :: String         <?> "Model specification file"
  , tmax        :: Double         <?> "Time to run the simulation"
  , csv         :: Maybe FilePath <?> "CSV output of simulation values"
  , granularity :: Maybe Double   <?> "Logging intervals"
  , verbose     :: Maybe Bool     <?> "Verbosity during simulation"
  , warmup      :: Maybe Double   <?> "Warmup time before logging"
  } deriving (Generic, Show)

instance ParseRecord Args
------------------------------------------------------------------------------
settings :: Args -> SSA.Model -> (SSA.SimulationSettings,SSA.SimState)
settings Args{..} s =
  ( SSA.SimulationSettings
    { SSA.system = s
    , SSA.tmax = unHelpful tmax
    , SSA.granularity = fromMaybe 10 (unHelpful granularity)
    , SSA.verbose = fromMaybe True (unHelpful verbose)
    , SSA.logFile = unHelpful csv
    }
  , SSA.SimState
    { SSA.state = SSA.initial s
    , SSA.time = - fromMaybe 0 (unHelpful warmup)
    , SSA.tlast = 0
    , SSA.slast = SSA.initial s
    }
  )

main :: IO ()
main = do
  args <- getRecord "Stochastic Simulation Algorithm"
  let modelFile = unHelpful (model args)
  modelStr <- readFile modelFile
  case SSA.parseModel modelFile modelStr of
    Left err -> print err
    Right m -> void . uncurry (execRWST SSA.simulation) $ settings args m
