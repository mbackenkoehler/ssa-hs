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
import qualified Data.List           as L
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Text.Read ( decimal )
import qualified Data.Vector.Unboxed as V
import           GHC.Generics
import           Options.Applicative
------------------------------------------------------------------------------
import qualified SSA.Parser as SSA
import qualified SSA.Model  as SSA
import qualified SSA.SSA    as SSA
------------------------------------------------------------------------------
data Verbosity = Normal | Verbose deriving (Eq)

data WriteTime = DoWriteTime | DontWriteTime deriving (Eq)

data Args = Args
  { model       :: String         -- Model specification file
  , times       :: [Double]       -- Time to run the simulation
  , csv         :: Maybe FilePath -- CSV output of simulation values
  , granularity :: Maybe Double   -- Logging intervals
  , verbose     :: Verbosity      -- Verbosity during simulation
  , warmup      :: Maybe Double   -- Warmup time before logging
  , runs        :: Maybe Int      -- Repeat simulation
  , start       :: Maybe String   -- Initial value sampels
  , writeTime   :: WriteTime      -- Write a time column
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
  <*> optional (strOption
      ( long "startvals"
     <> short 's'
     <> metavar "samples_file"
     <> help "Samples of the initial distribution (csv)"))
  <*> flag DoWriteTime DontWriteTime
      ( long "notime"
     <> help "Don't write a time column in the csv file")

opts = info (helper <*> args)
   ( fullDesc
  <> progDesc "Simulate a chemical reaction network specified in FILE"
  <> header "ssa-hs - Stochastic Simulation Algorithm" )


loggingMode :: Args -> Either Double [Double]
loggingMode Args{..} = if length times > 1
                          then Right times
                          else Left (fromMaybe 10 granularity)

initialSamples :: SSA.Model -> Maybe String -> ([SSA.State], Int)
initialSamples s ssamples =
  case ssamples of
    Nothing -> ([SSA.initial s], 1)
    Just dat -> parseSamples (SSA.names s) (T.pack dat)

parseSamples :: [T.Text] -> T.Text -> ([SSA.State], Int)
parseSamples names dat =
  let readInt txt = let Right n = decimal txt in fst n
      states = (fmap readInt . T.split (== ',')) <$> tail (T.lines dat)
      nameIdx n = let Just idx = L.findIndex (==n) names in idx
      res = [V.fromList [state !! (nameIdx n) | n <- names] | state <- states]
   in (res, length res)

settings :: Args -> SSA.Model -> Maybe String
         -> (SSA.SimulationSettings,SSA.SimState)
settings args@Args{..} s ssamples =
  let (startSamples, n) = initialSamples s ssamples
   in ( SSA.SimulationSettings
        { SSA.system = s
        , SSA.tmax = maximum times
        , SSA.loggingMode = loggingMode args
        , SSA.verbose = verbose == Verbose
        , SSA.logFile = csv
        , SSA.runs = max (fromMaybe 1 runs) 1
        , SSA.startSmpls = startSamples
        , SSA.nStartSmpls = n
        , SSA.writeTime = writeTime == DoWriteTime
        }
      , SSA.SimState
        { SSA.state = head startSamples
        , SSA.time = - fromMaybe 0 warmup
        , SSA.tlast = 0
        , SSA.slast = head startSamples
        }
      )

main :: IO ()
main = do
  args <- execParser opts
  let modelFile = model args
  modelStr <- readFile modelFile
  startSamples <- case start args of
                    Nothing -> pure Nothing
                    Just f -> Just <$> readFile f
  case SSA.parseModel modelFile modelStr of
    Left err -> print err
    Right m -> void . uncurry (execRWST SSA.simulation) $
      settings args m startSamples
