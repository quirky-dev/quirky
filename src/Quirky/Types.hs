{-# LANGUAGE DeriveAnyClass #-}

module Quirky.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

-- | Health check status
data HealthStatus
  = StatusOk
  | StatusWarning
  | StatusError
  deriving (Show, Eq, Generic)

instance ToJSON HealthStatus where
  toJSON StatusOk = String "ok"
  toJSON StatusWarning = String "warning"
  toJSON StatusError = String "error"

instance FromJSON HealthStatus where
  parseJSON = withText "HealthStatus" $ \case
    "ok" -> pure StatusOk
    "warning" -> pure StatusWarning
    "error" -> pure StatusError
    _ -> fail "Invalid status"

-- | Result of a health check
data HealthResult = HealthResult
  { hrStatus :: HealthStatus
  , hrMessage :: Text
  , hrData :: Value  -- Arbitrary JSON data
  } deriving (Show, Eq, Generic)

instance ToJSON HealthResult where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }
instance FromJSON HealthResult where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

-- | Context available during action execution
-- Stores results from previous actions for templating
type ActionContext = Map Text Value

-- | Configuration for an action (arbitrary JSON)
type ActionConfig = Value

-- | Built-in action types
data BuiltinAction
  = HttpGet
  | ParseJson
  | ParseHtml
  | RegexMatch
  | JsonExtract
  | SqlQuery
  | SshCommand
  | FileStat
  | CompareNumber
  | CompareTimestamp
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Action step in a check
data ActionStep = ActionStep
  { asName :: Maybe Text              -- ^ Optional name for this step (for referencing in context)
  , asAction :: ActionType            -- ^ Type of action
  , asConfig :: Maybe ActionConfig    -- ^ Optional configuration (can contain {{ template }} refs)
  } deriving (Show, Eq, Generic)

instance ToJSON ActionStep where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2, omitNothingFields = True }
instance FromJSON ActionStep where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

-- | Action can be builtin, named (from library/hub), or composite
data ActionType
  = BuiltinAction BuiltinAction
  | NamedAction Text                  -- ^ Named action (e.g., "system/check-disk")
  | CompositeAction [ActionStep]      -- ^ Composition of actions (for hub actions)
  deriving (Show, Eq, Generic)

instance ToJSON ActionType where
  toJSON (BuiltinAction ba) = object ["type" .= String "builtin", "name" .= ba]
  toJSON (NamedAction name) = object ["type" .= String "named", "name" .= name]
  toJSON (CompositeAction steps) = object ["type" .= String "composite", "steps" .= steps]

instance FromJSON ActionType where
  parseJSON v = parseAsString v <|> parseAsObject v
    where
      -- Support simple string syntax: action: "http_get" (builtin) or "system/check-disk" (named)
      parseAsString = withText "ActionType" $ \txt -> do
        -- First check if it has a slash - if so, it's a named action (or builtins/ prefix for backward compat)
        case T.stripPrefix "builtins/" txt of
          Just builtin -> case parseBuiltin builtin of  -- Legacy "builtins/" prefix
            Just action -> pure action
            Nothing -> fail $ "Unknown builtin action: " ++ T.unpack builtin
          Nothing -> case T.elem '/' txt of
            True -> pure $ NamedAction txt  -- Has slash, treat as named action
            False -> case parseBuiltin txt of  -- No slash, try as builtin first
              Just action -> pure action
              Nothing -> pure $ NamedAction txt  -- Not a builtin, treat as named action

      parseBuiltin builtin = case builtin of
        "http_get" -> Just $ BuiltinAction HttpGet
        "parse_json" -> Just $ BuiltinAction ParseJson
        "parse_html" -> Just $ BuiltinAction ParseHtml
        "regex_match" -> Just $ BuiltinAction RegexMatch
        "json_extract" -> Just $ BuiltinAction JsonExtract
        "sql_query" -> Just $ BuiltinAction SqlQuery
        "ssh_command" -> Just $ BuiltinAction SshCommand
        "file_stat" -> Just $ BuiltinAction FileStat
        "compare_number" -> Just $ BuiltinAction CompareNumber
        "compare_timestamp" -> Just $ BuiltinAction CompareTimestamp
        _ -> Nothing

      -- Support object syntax for builtins/composites
      parseAsObject = withObject "ActionType" $ \o -> do
        typ <- o .: "type" :: Parser Text
        case typ of
          "builtin" -> BuiltinAction <$> o .: "name"
          "named" -> NamedAction <$> o .: "name"
          "composite" -> CompositeAction <$> o .: "steps"
          _ -> fail $ "Unknown action type: " ++ show typ

-- | A complete health check definition
data HealthCheck = HealthCheck
  { hcActions :: [ActionStep]
  } deriving (Show, Eq, Generic)

instance ToJSON HealthCheck where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }
instance FromJSON HealthCheck where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

-- | Satellite configuration (runs checks, exposes /health endpoint)
data SatelliteConfig = SatelliteConfig
  { satPort :: Int
  , satBind :: Text
  , satChecks :: Map Text HealthCheck
  } deriving (Show, Eq, Generic)

instance ToJSON SatelliteConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }
instance FromJSON SatelliteConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }

-- | Remote satellite endpoint to poll
data SatelliteEndpoint = SatelliteEndpoint
  { seUrl :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON SatelliteEndpoint where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }
instance FromJSON SatelliteEndpoint where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

-- | Alert provider configuration
data AlertConfig = AlertConfig
  { acPushover :: Maybe PushoverConfig
  , acSlack :: Maybe SlackConfig
  } deriving (Show, Eq, Generic)

instance ToJSON AlertConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }
instance FromJSON AlertConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

data PushoverConfig = PushoverConfig
  { pcUserFile :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON PushoverConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }
instance FromJSON PushoverConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

data SlackConfig = SlackConfig
  { scWebhookFile :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON SlackConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }
instance FromJSON SlackConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

-- | Aggregator configuration (polls satellites, sends alerts, serves dashboard)
data AggregatorConfig = AggregatorConfig
  { aggPort :: Int
  , aggBind :: Text
  , aggInterval :: Int  -- Polling interval in seconds
  , aggSatellites :: Map Text SatelliteEndpoint
  , aggAlerts :: AlertConfig
  } deriving (Show, Eq, Generic)

instance ToJSON AggregatorConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }
instance FromJSON AggregatorConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }

-- | Top-level configuration that can be satellite, aggregator, or both
data Config = Config
  { cfgSatellite :: Maybe SatelliteConfig
  , cfgAggregator :: Maybe AggregatorConfig
  } deriving (Show, Eq, Generic)

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }

-- | Determine mode from config
data RunMode
  = SatelliteMode SatelliteConfig
  | AggregatorMode AggregatorConfig
  | BothMode SatelliteConfig AggregatorConfig
  deriving (Show, Eq)

detectMode :: Config -> Maybe RunMode
detectMode cfg = case (cfgSatellite cfg, cfgAggregator cfg) of
  (Just sc, Nothing) -> Just $ SatelliteMode sc
  (Nothing, Just ac) -> Just $ AggregatorMode ac
  (Just sc, Just ac) -> Just $ BothMode sc ac
  (Nothing, Nothing) -> Nothing

-- | Action execution error
data ActionError
  = ExecutionError Text
  | TemplatingError Text
  | ConfigError Text
  | TimeoutError
  deriving (Show, Eq)

-- | Aggregated status from all satellites (for aggregator)
data AggregatedStatus = AggregatedStatus
  { asTimestamp :: Text
  , asSatellites :: Map Text SatelliteStatus
  , asAllOk :: Bool
  } deriving (Show, Eq, Generic, FromJSON)

instance ToJSON AggregatedStatus where
  toJSON (AggregatedStatus ts sats ok) = object
    [ "last_update" .= ts
    , "satellites" .= sats
    , "all_ok" .= ok
    ]

-- | Status from a single satellite
data SatelliteStatus
  = SatelliteOk (Map Text HealthResult)
  | SatelliteError Text
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON SatelliteStatus where
  toJSON (SatelliteOk checks) = object
    [ "status" .= ("ok" :: Text)
    , "checks" .= checks
    ]
  toJSON (SatelliteError err) = object
    [ "status" .= ("error" :: Text)
    , "error" .= err
    ]

-- | Alert state tracking (to avoid duplicate alerts)
data AlertState = AlertState
  { asLastAlert :: Maybe Text  -- Last alert message sent
  , asLastAlertTime :: Int      -- Unix timestamp of last alert
  } deriving (Show, Eq)
