module Quirky.Alerts where

import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Quirky.Types

-- | Check if we should send an alert
shouldAlert :: AlertState -> AggregatedStatus -> Bool
shouldAlert state status =
  not (asAllOk status) &&  -- Something is wrong
  (asLastAlert state /= Just (buildAlertMessage status))  -- Message changed

-- | Build alert message from aggregated status
buildAlertMessage :: AggregatedStatus -> Text
buildAlertMessage status =
  let failures = collectFailures (asSatellites status)
  in T.intercalate ", " failures

-- | Collect failure messages from satellites
collectFailures :: Map Text SatelliteStatus -> [Text]
collectFailures satellites =
  concatMap extractFailures (Map.toList satellites)
  where
    extractFailures (satName, SatelliteError err) =
      [satName <> ": " <> err]
    extractFailures (satName, SatelliteOk checks) =
      map (\(checkName, result) -> satName <> "/" <> checkName <> ": " <> hrMessage result)
          (filter (\(_, r) -> hrStatus r /= StatusOk) $ Map.toList checks)

-- | Send alert via configured providers
sendAlert :: AlertConfig -> Text -> IO ()
sendAlert config message = runStdoutLoggingT $ do
  -- Send to Pushover if configured
  case acPushover config of
    Just pushoverCfg -> sendPushover pushoverCfg message
    Nothing -> return ()

  -- Send to Slack if configured
  case acSlack config of
    Just slackCfg -> sendSlack slackCfg message
    Nothing -> return ()

-- | Send Pushover notification
sendPushover :: (MonadIO m, MonadLogger m) => PushoverConfig -> Text -> m ()
sendPushover cfg message = do
  -- Hardcoded Quirky app token
  let token = "a2jxtpz1n1g4o11tpk6y2kgerx35ge"

  -- User key still comes from config file
  user <- liftIO $ TIO.readFile (T.unpack $ pcUserFile cfg)

  manager <- liftIO $ newManager tlsManagerSettings
  initialRequest <- liftIO $ parseRequest "https://api.pushover.net/1/messages.json"

  let requestBody = object
        [ "token" .= (token :: Text)
        , "user" .= T.strip user
        , "title" .= ("Quirky Alert" :: Text)
        , "message" .= message
        , "priority" .= (0 :: Int)
        ]

  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode requestBody
        , requestHeaders = [("Content-Type", "application/json")]
        }

  result <- liftIO $ (Right <$> httpLbs request manager) `catch` \(e :: SomeException) -> return (Left e)
  case result of
    Right response -> do
      let status = statusCode $ responseStatus response
      $logInfo $ "Pushover notification sent (status: " <> T.pack (show status) <> ")"
    Left e ->
      $logError $ "Failed to send Pushover alert: " <> T.pack (show e)

-- | Send Slack notification
sendSlack :: (MonadIO m, MonadLogger m) => SlackConfig -> Text -> m ()
sendSlack cfg message = do
  webhook <- liftIO $ TIO.readFile (T.unpack $ scWebhookFile cfg)

  manager <- liftIO $ newManager tlsManagerSettings
  initialRequest <- liftIO $ parseRequest (T.unpack $ T.strip webhook)

  let requestBody = object
        [ "text" .= ("*Quirky Alert*\n" <> message)
        ]

  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode requestBody
        , requestHeaders = [("Content-Type", "application/json")]
        }

  result <- liftIO $ (Right <$> httpLbs request manager) `catch` \(e :: SomeException) -> return (Left e)
  case result of
    Right response -> do
      let status = statusCode $ responseStatus response
      $logInfo $ "Slack alert sent (status: " <> T.pack (show status) <> ")"
    Left e ->
      $logError $ "Failed to send Slack alert: " <> T.pack (show e)

-- | Send recovery notification
sendRecovery :: AlertConfig -> IO ()
sendRecovery config = sendAlert config "All checks passing - system recovered"
