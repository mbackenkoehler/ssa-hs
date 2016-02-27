module Main where
------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict ( execRWST )
import qualified System.Environment as Sys
------------------------------------------------------------------------------
import           SSA.Parser
import           SSA.Model
import           SSA.SSA
------------------------------------------------------------------------------
settings :: Model -> Time -> Maybe FilePath -> SimulationSettings
settings sys tmax log = SimulationSettings
  { system = sys
  , tmax = tmax
  , granularity = 10
  , verbose = True
  , logFile = log
  }

simState :: Model -> SimState
simState sys = SimState
  { state = initial sys
  , time = 0
  , tlast = 0
  , slast = initial sys
  }

runSimulation :: Model -> Time -> FilePath -> IO Trajectory
runSimulation m t l =
  let simSettings = settings m t (Just l)
   in pure . snd =<< execRWST simulation simSettings (simState m)

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    modelFile : tmax' : log : _ -> do
      res <- readFile modelFile
      let tmax = read tmax' :: Double
      case parseModel modelFile res of
        Left err -> print err
        Right m -> do
          _ <- runSimulation m tmax log
          return ()
    _ -> putStrLn "provide a model file"
