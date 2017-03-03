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
import           Data.List              ( mapAccumL, find )
import           Data.Monoid
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           System.Random          ( randomIO, randomRIO )
import           Control.Monad
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Trans.RWS.Strict hiding ( state )
------------------------------------------------------------------------------
import           SSA.Model
------------------------------------------------------------------------------
type Time = Double

data SimState = SimState
  { state :: State
  , time  :: Time
  , tlast :: Time  -- ^ last logging time point
  , slast :: State -- ^ last logged state
  } deriving Show

type Trajectory = [State]

data SimulationSettings = SimulationSettings
  { system      :: Model
  , tmax        :: Time
  , loggingMode :: Either Time [Time]
  , verbose     :: Bool
  , logFile     :: Maybe FilePath
  , runs        :: Int
  }

type Simulation a = RWST SimulationSettings Trajectory SimState IO a
------------------------------------------------------------------------------
simulation :: Simulation ()
simulation = do
  startTime <- gets time
  logStart
  r <- asks runs
  replicateM_ r $ do
    runSimulation
    resetState startTime
  logEnd

resetState :: Time -> Simulation()
resetState t = do
  init <- initial <$> asks system
  put SimState { state = init
               , time  = t
               , tlast = 0
               , slast = init
               }

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
  successor <- (`applyChange` change r) <$> gets state
  modify $ \s -> s { state = successor, time = tsucc }

rates :: Simulation (Rate, [(Rate, Reaction)])
rates = computeRates <$> gets state <*> asks system

computeRates :: State -> Model -> (Rate, [(Rate, Reaction)])
computeRates s = mapAccumL f 0 . reactions
  where f a r = let r' = propensity r s in (a+r',(a+r',r))

timeStep :: MonadIO m => Rate -> m Time
timeStep l = toExpDist l <$> liftIO randomIO

toExpDist :: Rate -> Double -> Time
toExpDist l r = -log (1-r) / l

logState :: Simulation ()
logState = do
  SimState {..} <- get
  mode <- asks loggingMode
  let tnext = case mode of
                Left h -> tlast + h
                Right ts -> let future = [t | t <- ts, t > tlast]
                             in if length future == 0
                                   then time
                                   else minimum future
  when (time > tnext) $ do
    tmax <- asks tmax
    modify $ \s -> s { tlast = tnext }
    unless (tnext > tmax) $ writeLog tnext slast
    logState
  modify $ \s -> s { slast = state }

writeLog :: Time -> State -> Simulation ()
writeLog time state = do
  let stateStr = (<> "\n") . T.init . T.tail . T.pack . show $ state
      timeStr = T.justifyLeft 10 ' ' . T.pack . show . round $ time
      timeState = (T.pack . show $ time) <> "," <> stateStr
  verbose <- asks verbose
  when verbose . liftIO . T.putStr $ timeStr <> stateStr
  liftIO . maybe (pure ()) (`T.appendFile` timeState) =<< asks logFile

logStart :: Simulation ()
logStart = do
  liftIO (putStrLn "Running simulation...")
  mPath <- asks logFile
  case mPath of
    Nothing -> pure ()
    Just p -> csvHeader p =<< asks system

csvHeader :: MonadIO m => FilePath -> Model -> m ()
csvHeader f = liftIO . T.writeFile f . ("time," <>) . (<> "\n")
              . T.intercalate "," . names

logEnd :: Simulation ()
logEnd = liftIO . putStrLn . endMsg =<< asks logFile
  where endMsg = ("Simulation finished" <>) . maybe "" ((<> ")") . (" (" <>))
