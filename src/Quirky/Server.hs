{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Quirky.Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)
import Quirky.Engine
import Quirky.Types
import Servant

-- | API definition
type API = "health" :> Get '[JSON] HealthResponse

-- | Response containing all check results
data HealthResponse = HealthResponse
  { hrChecks :: Map Text HealthResult
  , hrAllOk :: Bool
  } deriving (Show, Generic)

instance ToJSON HealthResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

-- | Server implementation for satellite
server :: SatelliteConfig -> Server API
server config = healthHandler
  where
    healthHandler = liftIO $ do
      -- Execute all checks
      results <- mapM executeCheck (Map.elems $ satChecks config)

      -- Build response
      let checkResults = Map.fromList $ zip (Map.keys $ satChecks config) (map extractResult results)
          allOk = all (\r -> hrStatus r == StatusOk) (Map.elems checkResults)

      return $ HealthResponse checkResults allOk

    extractResult :: Either ActionError HealthResult -> HealthResult
    extractResult (Right r) = r
    extractResult (Left err) =
      -- Note: detailed error already logged in Engine.hs
      HealthResult
        { hrStatus = StatusError
        , hrMessage = "Execution failed: " <> T.pack (show err)
        , hrData = Null
        }

-- | Run the satellite server
runSatellite :: SatelliteConfig -> IO ()
runSatellite config = runStdoutLoggingT $ do
  $logInfo $ "Starting Quirky satellite on " <> satBind config <> ":" <> T.pack (show (satPort config))
  liftIO $ run (satPort config) $ serve (Proxy :: Proxy API) (server config)
