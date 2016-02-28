{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
------------------------------------------------------------------------------
{-|
  Module      : SSA.SSA
  Description : Stochastic Simulation Algorithm
  Copyright   : 2016 Michael Backenköhler
  License     : MIT
  Maintainer  : Michael Backenköhler
  Stability   : experimental
  Portability : portable
-}
------------------------------------------------------------------------------
module SSA.SSA
    ( Time, SimState(..), Trajectory, SimulationSettings(..), Simulation
    , simulation
    ) where
------------------------------------------------------------------------------
import           Data.List              ( mapAccumL, find, intercalate )
import           Data.Monoid
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           System.Random          ( randomRIO )
import           Control.Monad
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.RWS.Strict hiding ( state )
------------------------------------------------------------------------------
import           SSA.Model
------------------------------------------------------------------------------
type Time = Double

data SimState = SimState
  { state :: !State
  , time  :: {-# UNPACK #-} !Time
  , tlast :: {-# UNPACK #-} !Time -- ^ last logging time point
  , slast :: !State               -- ^ last logged state
  } deriving Show

type Trajectory = [State]

data SimulationSettings = SimulationSettings
  { system      :: Model
  , tmax        :: {-# UNPACK #-} !Time
  , granularity :: {-# UNPACK #-} !Time
  , verbose     :: !Bool
  , logFile     :: Maybe FilePath
  }

type Simulation = RWST SimulationSettings Trajectory SimState IO ()
------------------------------------------------------------------------------
simulation :: Simulation
simulation = do
  logState
  step
  tmax <- asks tmax
  time <- gets time
  if (time > tmax) then logState else simulation

step :: Simulation
step = do
  s <- gets state
  sys <- asks system
  let f a r = let r' = propensity r s in (a+r',(a+r',r))
      (rsum,rs) = mapAccumL f 0 $ reactions sys
  rand <- liftIO $ randomRIO (0,rsum)
  dt <- liftIO $ timeStep rsum
  t <- gets time
  let Just (_,r) = find ((>rand) . fst) rs
      succ = applyChange s $ change r
  modify $ \s -> s { state = succ, time = t+dt }

timeStep :: Rate -> IO Time
timeStep l = (\r -> -log (1-r) / l) <$> randomRIO (0,1)

logState :: Simulation
logState = do
    SimState {..} <- get
    h <- asks granularity
    let tnext = tlast + h
    when (time >= tnext) $ do
      tmax <- asks tmax
      unless (tnext > tmax) $ writeLog tnext slast
      modify $ \s -> s { tlast = tnext }
    when (time >= tnext) logState
    modify $ \s -> s { slast = state }

writeLog :: Time -> State -> Simulation
writeLog time state = do
    let stateStr = (T.init . T.tail . T.pack . show) state <> "\n"
        timeStr = T.justifyLeft 10 ' ' . T.pack . show . round $ time
    verbose <- asks verbose
    when verbose $ liftIO $ T.putStr $ timeStr <> stateStr
    logfile <- asks logFile
    liftIO $ maybe (pure ()) (flip T.appendFile stateStr) logfile

initLogFile :: FilePath -> Model -> IO ()
initLogFile file =
    writeFile file . (<> "\n") . intercalate "," . fmap T.unpack . names
