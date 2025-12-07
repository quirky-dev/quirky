module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Yaml (decodeFileEither)
import Quirky.Aggregator
import Quirky.Server
import Quirky.Types
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath] -> do
      result <- decodeFileEither configPath
      case result of
        Left err -> do
          putStrLn $ "Failed to parse config: " ++ show err
          exitFailure
        Right config -> do
          putStrLn $ "Parsed config: " ++ show config
          case detectMode config of
            Nothing -> do
              putStrLn "Error: Config must have either 'satellite' or 'aggregator' section"
              exitFailure

            Just (SatelliteMode satCfg) -> do
              putStrLn "Running in SATELLITE mode"
              runSatellite satCfg

            Just (AggregatorMode aggCfg) -> do
              putStrLn "Running in AGGREGATOR mode"
              runAggregator aggCfg

            Just (BothMode satCfg aggCfg) -> do
              putStrLn "Running in BOTH modes (satellite + aggregator)"
              -- Start satellite in background thread
              void $ forkIO $ runSatellite satCfg
              -- Run aggregator in main thread
              runAggregator aggCfg

    _ -> do
      putStrLn "Usage: quirky <config.yaml>"
      exitFailure
