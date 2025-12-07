{-# LANGUAGE QuasiQuotes #-}

module ConfigParsingTests (tests) where

import Data.Aeson (decode, encode, eitherDecode)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Map.Strict as Map
import Quirky.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Config Parsing Tests"
  [ healthStatusTests
  , actionTypeTests
  , healthCheckTests
  , satelliteConfigTests
  , aggregatorConfigTests
  , topLevelConfigTests
  , runModeTests
  ]

-- | Test HealthStatus serialization
healthStatusTests :: TestTree
healthStatusTests = testGroup "HealthStatus"
  [ testCase "parses 'ok'" $
      decode "\"ok\"" @?= Just StatusOk
  , testCase "parses 'warning'" $
      decode "\"warning\"" @?= Just StatusWarning
  , testCase "parses 'error'" $
      decode "\"error\"" @?= Just StatusError
  , testCase "rejects invalid status" $
      (decode "\"invalid\"" :: Maybe HealthStatus) @?= Nothing
  , testCase "serializes StatusOk" $
      encode StatusOk @?= "\"ok\""
  , testCase "serializes StatusError" $
      encode StatusError @?= "\"error\""
  ]

-- | Test ActionType parsing (builtin, named, composite)
actionTypeTests :: TestTree
actionTypeTests = testGroup "ActionType"
  [ testGroup "Builtin Actions"
      [ testCase "parses http_get as builtin" $
          decode "\"http_get\"" @?= Just (BuiltinAction HttpGet)
      , testCase "parses parse_json as builtin" $
          decode "\"parse_json\"" @?= Just (BuiltinAction ParseJson)
      , testCase "parses compare_number as builtin" $
          decode "\"compare_number\"" @?= Just (BuiltinAction CompareNumber)
      , testCase "parses legacy builtins/ prefix" $
          decode "\"builtins/http_get\"" @?= Just (BuiltinAction HttpGet)
      ]
  , testGroup "Named Actions"
      [ testCase "parses action with slash as named" $
          decode "\"system/check-disk\"" @?= Just (NamedAction "system/check-disk")
      , testCase "parses custom namespace" $
          decode "\"custom/my-action\"" @?= Just (NamedAction "custom/my-action")
      , testCase "parses unknown non-slash string as named action" $
          decode "\"unknown_action\"" @?= Just (NamedAction "unknown_action")
      ]
  , testGroup "Object Syntax"
      [ testCase "parses builtin object syntax" $
          (eitherDecode "{\"type\": \"builtin\", \"name\": \"HttpGet\"}" :: Either String ActionType)
          @?= Right (BuiltinAction HttpGet)
      , testCase "parses named object syntax" $
          (eitherDecode "{\"type\": \"named\", \"name\": \"system/check-disk\"}" :: Either String ActionType)
          @?= Right (NamedAction "system/check-disk")
      ]
  ]

-- | Test HealthCheck parsing
healthCheckTests :: TestTree
healthCheckTests = testGroup "HealthCheck"
  [ testCase "parses simple health check with one action" $ do
      let json = [aesonQQ|
        {
          "actions": [
            { "action": "http_get", "config": { "url": "https://example.com" } }
          ]
        }
        |]
      case decode (encode json) of
        Just (hc :: HealthCheck) -> length (hcActions hc) @?= 1
        Nothing -> assertFailure "Failed to parse"

  , testCase "parses health check with multiple actions" $ do
      let json = [aesonQQ|
        {
          "actions": [
            { "action": "http_get", "config": { "url": "https://api.example.com" } },
            { "action": "parse_json" },
            { "action": "json_extract", "config": { "path": "$.status" } }
          ]
        }
        |]
      case decode (encode json) of
        Just (hc :: HealthCheck) -> length (hcActions hc) @?= 3
        Nothing -> assertFailure "Failed to parse multi-action check"

  , testCase "parses health check with named steps" $ do
      let json = [aesonQQ|
        {
          "actions": [
            { "name": "fetch", "action": "http_get", "config": { "url": "https://api.example.com" } },
            { "name": "parse", "action": "parse_json" }
          ]
        }
        |]
      case decode (encode json) of
        Just (hc :: HealthCheck) -> do
          let steps = hcActions hc
          length steps @?= 2
          case steps of
            (first:second:_) -> do
              asName first @?= Just "fetch"
              asName second @?= Just "parse"
            _ -> assertFailure "Expected at least 2 steps"
        Nothing -> assertFailure "Failed to parse named steps"
  ]

-- | Test SatelliteConfig parsing
satelliteConfigTests :: TestTree
satelliteConfigTests = testGroup "SatelliteConfig"
  [ testCase "parses basic satellite config" $ do
      let json = [aesonQQ|
        {
          "port": 8080,
          "bind": "0.0.0.0",
          "checks": {
            "web": { "actions": [{ "action": "http_get" }] }
          }
        }
        |]
      case decode (encode json) of
        Just (cfg :: SatelliteConfig) -> do
          satPort cfg @?= 8080
          satBind cfg @?= "0.0.0.0"
          Map.size (satChecks cfg) @?= 1
        Nothing -> assertFailure "Failed to parse"

  , testCase "parses satellite with multiple checks" $ do
      let json = [aesonQQ|
        {
          "port": 3000,
          "bind": "127.0.0.1",
          "checks": {
            "web": { "actions": [{ "action": "http_get" }] },
            "db": { "actions": [{ "action": "sql_query" }] },
            "disk": { "actions": [{ "action": "file_stat" }] }
          }
        }
        |]
      case decode (encode json) of
        Just (cfg :: SatelliteConfig) -> do
          satPort cfg @?= 3000
          Map.size (satChecks cfg) @?= 3
          Map.member "web" (satChecks cfg) @?= True
          Map.member "db" (satChecks cfg) @?= True
          Map.member "disk" (satChecks cfg) @?= True
        Nothing -> assertFailure "Failed to parse multi-check satellite"
  ]

-- | Test AggregatorConfig parsing
aggregatorConfigTests :: TestTree
aggregatorConfigTests = testGroup "AggregatorConfig"
  [ testCase "parses basic aggregator config" $ do
      let json = [aesonQQ|
        {
          "port": 9000,
          "bind": "0.0.0.0",
          "interval": 60,
          "satellites": {
            "server1": { "url": "http://server1:8080/health" }
          },
          "alerts": {}
        }
        |]
      case decode (encode json) of
        Just (cfg :: AggregatorConfig) -> do
          aggPort cfg @?= 9000
          aggBind cfg @?= "0.0.0.0"
          aggInterval cfg @?= 60
          Map.size (aggSatellites cfg) @?= 1
        Nothing -> assertFailure "Failed to parse aggregator config"

  , testCase "parses aggregator with Pushover alerts" $ do
      let json = [aesonQQ|
        {
          "port": 9000,
          "bind": "0.0.0.0",
          "interval": 30,
          "satellites": {},
          "alerts": {
            "pushover": { "user_file": "/secrets/pushover-user.txt" }
          }
        }
        |]
      case decode (encode json) of
        Just (cfg :: AggregatorConfig) -> do
          let alerts = aggAlerts cfg
          case acPushover alerts of
            Just pc -> pcUserFile pc @?= "/secrets/pushover-user.txt"
            Nothing -> assertFailure "Pushover config not parsed"
        Nothing -> assertFailure "Failed to parse aggregator with alerts"

  , testCase "parses aggregator with Slack alerts" $ do
      let json = [aesonQQ|
        {
          "port": 9000,
          "bind": "0.0.0.0",
          "interval": 30,
          "satellites": {},
          "alerts": {
            "slack": { "webhook_file": "/secrets/slack-webhook.txt" }
          }
        }
        |]
      case decode (encode json) of
        Just (cfg :: AggregatorConfig) -> do
          let alerts = aggAlerts cfg
          case acSlack alerts of
            Just sc -> scWebhookFile sc @?= "/secrets/slack-webhook.txt"
            Nothing -> assertFailure "Slack config not parsed"
        Nothing -> assertFailure "Failed to parse aggregator with Slack"
  ]

-- | Test top-level Config parsing
topLevelConfigTests :: TestTree
topLevelConfigTests = testGroup "Top-level Config"
  [ testCase "parses satellite-only config" $ do
      let json = [aesonQQ|
        {
          "satellite": {
            "port": 8080,
            "bind": "0.0.0.0",
            "checks": {}
          }
        }
        |]
      case decode (encode json) of
        Just (cfg :: Config) -> do
          cfgSatellite cfg @?= Just (SatelliteConfig 8080 "0.0.0.0" Map.empty)
          cfgAggregator cfg @?= Nothing
        Nothing -> assertFailure "Failed to parse satellite-only config"

  , testCase "parses aggregator-only config" $ do
      let json = [aesonQQ|
        {
          "aggregator": {
            "port": 9000,
            "bind": "0.0.0.0",
            "interval": 60,
            "satellites": {},
            "alerts": {}
          }
        }
        |]
      case decode (encode json) of
        Just (cfg :: Config) -> do
          cfgSatellite cfg @?= Nothing
          case cfgAggregator cfg of
            Just _ -> return ()
            Nothing -> assertFailure "Aggregator config not parsed"
        Nothing -> assertFailure "Failed to parse aggregator-only config"

  , testCase "parses combined satellite + aggregator config" $ do
      let json = [aesonQQ|
        {
          "satellite": {
            "port": 8080,
            "bind": "0.0.0.0",
            "checks": {}
          },
          "aggregator": {
            "port": 9000,
            "bind": "0.0.0.0",
            "interval": 60,
            "satellites": {},
            "alerts": {}
          }
        }
        |]
      case decode (encode json) of
        Just (cfg :: Config) -> do
          case cfgSatellite cfg of
            Just _ -> return ()
            Nothing -> assertFailure "Satellite config not parsed"
          case cfgAggregator cfg of
            Just _ -> return ()
            Nothing -> assertFailure "Aggregator config not parsed"
        Nothing -> assertFailure "Failed to parse combined config"
  ]

-- | Test RunMode detection
runModeTests :: TestTree
runModeTests = testGroup "RunMode Detection"
  [ testCase "detects SatelliteMode" $ do
      let cfg = Config (Just $ SatelliteConfig 8080 "0.0.0.0" Map.empty) Nothing
      case detectMode cfg of
        Just (SatelliteMode _) -> return ()
        _ -> assertFailure "Failed to detect SatelliteMode"

  , testCase "detects AggregatorMode" $ do
      let cfg = Config Nothing (Just $ AggregatorConfig 9000 "0.0.0.0" 60 Map.empty (AlertConfig Nothing Nothing))
      case detectMode cfg of
        Just (AggregatorMode _) -> return ()
        _ -> assertFailure "Failed to detect AggregatorMode"

  , testCase "detects BothMode" $ do
      let sat = SatelliteConfig 8080 "0.0.0.0" Map.empty
      let agg = AggregatorConfig 9000 "0.0.0.0" 60 Map.empty (AlertConfig Nothing Nothing)
      let cfg = Config (Just sat) (Just agg)
      case detectMode cfg of
        Just (BothMode _ _) -> return ()
        _ -> assertFailure "Failed to detect BothMode"

  , testCase "returns Nothing for empty config" $ do
      let cfg = Config Nothing Nothing
      detectMode cfg @?= Nothing
  ]
