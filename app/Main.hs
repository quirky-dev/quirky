module Main where

import Control.Concurrent (forkIO)
import Control.Exception (catch)
import Control.Monad (void)
import Control.Monad.Logger
import Data.Text (pack)
import Data.Yaml (decodeFileEither)
import Foreign.C.Types (CInt(..))
import Quirky.Aggregator
import Quirky.Logger
import Quirky.Server
import Quirky.Types
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)

foreign import ccall "exit" c_exit :: CInt -> IO ()

main :: IO ()
main = run `catch` handleExit
  where
    handleExit :: ExitCode -> IO ()
    handleExit ExitSuccess = c_exit 0
    handleExit (ExitFailure n) = c_exit (fromIntegral n)

run :: IO ()
run = do
  args <- getArgs
  case args of
    [configPath] -> do
      result <- decodeFileEither configPath
      case result of
        Left err -> withLogging $ logErrorN $ pack $ "Failed to parse config: " <> show err
        Right config -> do
          withLogging $ logDebugN $ pack $ "Parsed config: " <> show config
          case detectMode config of
            Nothing ->
              withLogging $ logErrorN "Config must have either 'satellite' or 'aggregator' section"

            Just (SatelliteMode satCfg) ->
              runSatellite satCfg

            Just (AggregatorMode aggCfg) ->
              runAggregator aggCfg

            Just (BothMode satCfg aggCfg) -> do
              -- Start satellite in background thread
              void $ forkIO $ runSatellite satCfg
              -- Run aggregator in main thread
              runAggregator aggCfg

    _ ->
      hPutStrLn stderr "Usage: quirky <config.yaml>"
