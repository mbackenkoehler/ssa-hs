{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
------------------------------------------------------------------------------
module Main where
------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict ( execRWST )
import           Data.Maybe
import qualified Data.Text as T
import           Options.Generic
import qualified System.Environment as Sys
------------------------------------------------------------------------------
import qualified SSA.Parser as SSA
import qualified SSA.Model  as SSA
import qualified SSA.SSA    as SSA
------------------------------------------------------------------------------
data Args = Args
  { model       :: String
  , tmax        :: Double
  , csv         :: Maybe FilePath
  , granularity :: Maybe Double
  , verbose     :: Maybe Bool
  , warmup      :: Maybe Double
  } deriving (Generic, Show)

instance ParseRecord Args
------------------------------------------------------------------------------
settingsAndState :: Args -> SSA.Model -> (SSA.SimulationSettings,SSA.SimState)
settingsAndState Args{..} s =
  ( SSA.SimulationSettings
    { SSA.system = s
    , SSA.tmax = tmax
    , SSA.granularity = fromMaybe 10 granularity
    , SSA.verbose = fromMaybe True verbose
    , SSA.logFile = csv
    }
  , SSA.SimState
    { SSA.state = SSA.initial s
    , SSA.time = - fromMaybe 0 warmup
    , SSA.tlast = 0
    , SSA.slast = SSA.initial s
    }
  )

main = do
  args <- getRecord "Stochastic Simulation Algorithm"
  modelStr <- readFile $ model args
  case SSA.parseModel (model args) modelStr of
    Left err -> print err
    Right m -> do
      let (settings, state) = settingsAndState args m
      _ <- execRWST SSA.simulation settings state
      putStrLn "Simulation finished"
