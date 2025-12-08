{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Quirky.Aggregator where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch, SomeException, IOException)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Aeson
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Streaming.Network (HostPreference)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort, runSettings)
import Quirky.Alerts
import Quirky.Logger
import Quirky.Types
import Servant
import qualified Servant.Server.StaticFiles as Static
import System.Directory (doesDirectoryExist)
import System.Environment (getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)

-- | Aggregator API: status endpoint, test alert, and static frontend
type AggregatorAPI =
       "api" :> "status" :> Get '[JSON] AggregatedStatus
  :<|> "api" :> "test-alert" :> Post '[JSON] Text
  :<|> Raw  -- Serve static frontend files (must be last)

-- | Shared state containing latest poll results and alert state
data AggregatorState = AggregatorState
  { aggStatus :: IORef AggregatedStatus
  , aggAlertState :: IORef AlertState
  , aggConfig :: AggregatorConfig
  }

-- | Response from satellite /health endpoint
data HealthResponse = HealthResponse
  { hrChecks :: Map Text HealthResult
  , hrAllOk :: Bool
  } deriving (Show, Generic)

instance FromJSON HealthResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

-- | Ensure URL ends with /health
ensureHealthPath :: Text -> Text
ensureHealthPath url
  | "/health" `T.isSuffixOf` url = url
  | otherwise = url <> "/health"

-- | Poll a satellite endpoint
pollSatellite :: Manager -> SatelliteEndpoint -> IO SatelliteStatus
pollSatellite manager endpoint = do
  let fullUrl = ensureHealthPath (seUrl endpoint)
  request <- parseRequest $ T.unpack fullUrl
  response <- httpLbs request manager
  case decode (responseBody response) of
    Just healthResp -> return $ SatelliteOk (hrChecks healthResp)
    Nothing -> return $ SatelliteError "Failed to parse response"
  `catch` \(e :: SomeException) ->
    return $ SatelliteError $ T.pack $ show e

-- | Poll all satellites and update state
pollAllSatellites :: Manager -> AggregatorConfig -> AggregatorState -> IO ()
pollAllSatellites manager config stateRef = forever $ do
  -- Poll each satellite
  let satellitesList = Map.toList (aggSatellites config)
  results <- mapM (\(name, endpoint) -> (name,) <$> pollSatellite manager endpoint) satellitesList

  -- Get current timestamp
  now <- getPOSIXTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (posixSecondsToUTCTime now)

  -- Build aggregated status
  let satelliteMap = Map.fromList results
      allOk = all isSatelliteOk (Map.elems satelliteMap)
      status = AggregatedStatus
        { asTimestamp = T.pack timestamp
        , asSatellites = satelliteMap
        , asAllOk = allOk
        }

  -- Update status
  writeIORef (aggStatus stateRef) status

  -- Handle alerts
  alertState <- readIORef (aggAlertState stateRef)
  if shouldAlert alertState status
    then do
      -- Send alert
      sendAlert (aggAlerts config) (buildAlertMessage status)
      -- Update alert state
      nowTime <- round <$> getPOSIXTime
      writeIORef (aggAlertState stateRef) AlertState
        { asLastAlert = Just (buildAlertMessage status)
        , asLastAlertTime = nowTime
        }
    else when (allOk && isJust (asLastAlert alertState)) $ do
      -- Send recovery
      sendRecovery (aggAlerts config)
      -- Clear alert state
      writeIORef (aggAlertState stateRef) AlertState
        { asLastAlert = Nothing
        , asLastAlertTime = 0
        }

  -- Sleep until next poll
  threadDelay (aggInterval config * 1000000)

isSatelliteOk :: SatelliteStatus -> Bool
isSatelliteOk (SatelliteOk checks) = all (\r -> hrStatus r == StatusOk) (Map.elems checks)
isSatelliteOk (SatelliteError _) = False

-- | Serve the aggregator dashboard and API
aggregatorServer :: FilePath -> AggregatorState -> Server AggregatorAPI
aggregatorServer webRoot state = statusHandler :<|> testAlertHandler :<|> Static.serveDirectoryFileServer webRoot
  where
    statusHandler = liftIO $ readIORef (aggStatus state)

    testAlertHandler = liftIO $ runStdoutLoggingT $ do
      $logInfo "Test alert triggered via API"
      liftIO $ sendAlert (aggAlerts $ aggConfig state) "This is a test alert from Quirky"
      return "Test alert sent successfully"

-- | Run the aggregator
runAggregator :: AggregatorConfig -> IO ()
runAggregator config = do
  let host = aggBind config
      port = aggPort config

  withLogging $ do
    logInfoN $ "Starting Quirky aggregator on " <> host <> ":" <> T.pack (show port)
    logInfoN $ "Polling " <> T.pack (show (Map.size $ aggSatellites config)) <> " satellites every " <> T.pack (show (aggInterval config)) <> "s"

  -- Initialize state
  let initialStatus = AggregatedStatus
        { asTimestamp = ""
        , asSatellites = Map.empty
        , asAllOk = True
        }
      initialAlertState = AlertState
        { asLastAlert = Nothing
        , asLastAlertTime = 0
        }

  statusRef <- newIORef initialStatus
  alertStateRef <- newIORef initialAlertState

  let state = AggregatorState
        { aggStatus = statusRef
        , aggAlertState = alertStateRef
        , aggConfig = config
        }

  -- Determine web root path
  exePath <- getExecutablePath
  let exeDir = takeDirectory exePath
      packageRoot = takeDirectory exeDir
      nixWebRoot = packageRoot </> "share" </> "quirky" </> "web"
      devWebRoot = "web" </> "dist"  -- For local development with cabal

  -- Check if nix-built web root exists, otherwise fall back to dev
  nixExists <- doesDirectoryExist nixWebRoot
  let webRoot = if nixExists then nixWebRoot else devWebRoot

  withLogging $ logDebugN $ "Serving frontend from: " <> T.pack webRoot

  -- Start polling thread
  manager <- newManager tlsManagerSettings
  _ <- forkIO $ pollAllSatellites manager config state

  -- Start web server
  let hostPref = fromString (T.unpack host) :: HostPreference
      settings = setPort port $ setHost hostPref defaultSettings
      app = serve (Proxy :: Proxy AggregatorAPI) (aggregatorServer webRoot state)

  runSettings settings app `catch` \(e :: IOException) -> do
    withLogging $ logErrorN $ "Failed to start server on " <> host <> ":" <> T.pack (show port) <> " - " <> T.pack (show e)
    exitFailure
