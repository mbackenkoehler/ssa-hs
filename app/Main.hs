{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
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
import           Options.Applicative
------------------------------------------------------------------------------
import qualified SSA.Parser as SSA
import qualified SSA.Model  as SSA
import qualified SSA.SSA    as SSA
------------------------------------------------------------------------------
data Verbosity = Normal | Verbose deriving (Eq)

data Args = Args
  { model       :: String         -- Model specification file
  , times       :: [Double]       -- Time to run the simulation
  , csv         :: Maybe FilePath -- CSV output of simulation values
  , granularity :: Maybe Double   -- Logging intervals
  , verbose     :: Verbosity      -- Verbosity during simulation
  , warmup      :: Maybe Double   -- Warmup time before logging
  , runs        :: Maybe Int      -- Repeat simulation
  }
------------------------------------------------------------------------------
args :: Parser Args
args = Args
  <$> strOption
      ( long "model"
     <> short 'm'
     <> metavar "FILE"
     <> help "Model specification file" )
  <*> some (option auto
      ( long "times"
     <> short 't'
     <> metavar "TIME"
     <> help "Time to run the simulation" ))
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "LOG_FILE"
     <> help "csv output file" ))
  <*> optional (option auto
      ( long "granularity"
     <> short 'g'
     <> metavar "dt"
     <> help "Logging intervals" ))
  <*> flag Normal Verbose
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose simulation" )
  <*> optional (option auto
      ( long "warmup"
     <> short 'w'
     <> help "Warmup time before logging starts" ))
  <*> optional (option auto
      ( long "repeat"
     <> short 'r'
     <> metavar "N"
     <> help "Repeat the simulation N times"))

opts = info (helper <*> args)
   ( fullDesc
  <> progDesc "Simulate a chemical reaction network specified in FILE"
  <> header "ssa-hs - Stochastic Simulation Algorithm" )


loggingMode :: Args -> Either Double [Double]
loggingMode Args{..} = if length times > 1
                          then Right times
                          else Left (fromMaybe 10 granularity)

settings :: Args -> SSA.Model -> (SSA.SimulationSettings,SSA.SimState)
settings args@Args{..} s =
  ( SSA.SimulationSettings
    { SSA.system = s
    , SSA.tmax = maximum times
    , SSA.loggingMode = loggingMode args
    , SSA.verbose = verbose == Verbose
    , SSA.logFile = csv
    , SSA.runs = max (fromMaybe 1 runs) 1
    }
  , SSA.SimState
    { SSA.state = SSA.initial s
    , SSA.time = - fromMaybe 0 warmup
    , SSA.tlast = 0
    , SSA.slast = SSA.initial s
    }
  )

main :: IO ()
main = do
  args <- execParser opts
  let modelFile = model args
  modelStr <- readFile modelFile
  case SSA.parseModel modelFile modelStr of
    Left err -> print err
    Right m -> void . uncurry (execRWST SSA.simulation) $ settings args m
