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
import           Control.Monad.IO.Class ( MonadIO, liftIO )
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

type Simulation a = RWST SimulationSettings Trajectory SimState IO a
------------------------------------------------------------------------------
simulation :: Simulation ()
simulation = do
  mPath <- asks logFile
  case mPath of
    Nothing -> pure ()
    Just path -> initLog path =<< asks system
  runSimulation

runSimulation :: Simulation ()
runSimulation = do
  logState >> step
  tmax <- asks tmax
  time <- gets time
  if time > tmax then logState else runSimulation

step :: Simulation ()
step = do
  (rsum, rs) <- rates
  tsucc <- (+) <$> gets time <*> timeStep rsum
  rand <- liftIO $ randomRIO (0,rsum)
  let Just (_,r) = find ((>rand) . fst) rs
  succ <- (`applyChange` change r) <$> gets state
  modify $ \s -> s { state = succ, time = tsucc }

rates :: Simulation (Rate, [(Rate, Reaction)])
rates = do
  s <- gets state
  let f a r = let r' = propensity r s in (a+r',(a+r',r))
  mapAccumL f 0 . reactions <$> asks system

timeStep :: MonadIO m => Rate -> m Time
timeStep l = (\r -> -log (1-r) / l) <$> liftIO (randomRIO (0,1))

logState :: Simulation ()
logState = do
  SimState {..} <- get
  h <- asks granularity
  let tnext = tlast + h
  when (time >= tnext) $ do
    tmax <- asks tmax
    modify $ \s -> s { tlast = tnext }
    unless (tnext > tmax) $ writeLog tnext slast
    logState
  modify $ \s -> s { slast = state }

writeLog :: Time -> State -> Simulation ()
writeLog time state = do
  let stateStr = (<> "\n") . T.init . T.tail . T.pack . show $ state
      timeStr = T.justifyLeft 10 ' ' . T.pack . show . round $ time
  verbose <- asks verbose
  when verbose . liftIO . T.putStr $ timeStr <> stateStr
  liftIO . maybe (pure ()) (`T.appendFile` stateStr) =<< asks logFile

initLog :: MonadIO m => FilePath -> Model -> m ()
initLog f = liftIO . T.writeFile f . (<> "\n") . T.intercalate "," . names
