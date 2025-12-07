module AlertTests (tests) where

import Data.Aeson (object)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Quirky.Alerts
import Quirky.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Alert Tests"
  [ shouldAlertTests
  , buildMessageTests
  , collectFailuresTests
  ]

-- | Test shouldAlert logic
shouldAlertTests :: TestTree
shouldAlertTests = testGroup "shouldAlert Logic"
  [ testCase "should alert when all not ok and message different" $ do
      let state = AlertState Nothing 0
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteError "Connection failed")]
            , asAllOk = False
            }
      shouldAlert state status @?= True

  , testCase "should not alert when all ok" $ do
      let state = AlertState Nothing 0
      let checks = Map.fromList [("check1", HealthResult StatusOk "OK" (object []))]
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteOk checks)]
            , asAllOk = True
            }
      shouldAlert state status @?= False

  , testCase "should not alert when message unchanged" $ do
      let message = "sat1: Connection failed"
      let state = AlertState (Just message) 0
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteError "Connection failed")]
            , asAllOk = False
            }
      shouldAlert state status @?= False

  , testCase "should alert when message changed" $ do
      let state = AlertState (Just "old message") 0
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteError "New error")]
            , asAllOk = False
            }
      shouldAlert state status @?= True

  , testCase "should alert on first failure (no previous message)" $ do
      let state = AlertState Nothing 0
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteError "Error")]
            , asAllOk = False
            }
      shouldAlert state status @?= True
  ]

-- | Test buildAlertMessage
buildMessageTests :: TestTree
buildMessageTests = testGroup "buildAlertMessage"
  [ testCase "builds message for satellite error" $ do
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteError "Connection timeout")]
            , asAllOk = False
            }
      let message = buildAlertMessage status
      message @?= "sat1: Connection timeout"

  , testCase "builds message for multiple satellite errors" $ do
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList
                [ ("sat1", SatelliteError "Error 1")
                , ("sat2", SatelliteError "Error 2")
                ]
            , asAllOk = False
            }
      let message = buildAlertMessage status
      -- Messages can be in any order due to Map ordering
      T.isInfixOf "sat1: Error 1" message @?= True
      T.isInfixOf "sat2: Error 2" message @?= True

  , testCase "builds message for failed health check" $ do
      let checks = Map.fromList
            [ ("check1", HealthResult StatusError "Service down" (object []))
            ]
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteOk checks)]
            , asAllOk = False
            }
      let message = buildAlertMessage status
      message @?= "sat1/check1: Service down"

  , testCase "includes only failed checks, not ok ones" $ do
      let checks = Map.fromList
            [ ("check1", HealthResult StatusOk "OK" (object []))
            , ("check2", HealthResult StatusError "Failed" (object []))
            ]
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteOk checks)]
            , asAllOk = False
            }
      let message = buildAlertMessage status
      message @?= "sat1/check2: Failed"
      T.isInfixOf "check1" message @?= False

  , testCase "builds message with multiple failed checks" $ do
      let checks = Map.fromList
            [ ("check1", HealthResult StatusError "Error 1" (object []))
            , ("check2", HealthResult StatusWarning "Warning" (object []))
            ]
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteOk checks)]
            , asAllOk = False
            }
      let message = buildAlertMessage status
      T.isInfixOf "check1" message @?= True
      T.isInfixOf "check2" message @?= True

  , testCase "handles empty failures" $ do
      let checks = Map.fromList [("check1", HealthResult StatusOk "OK" (object []))]
      let status = AggregatedStatus
            { asTimestamp = "2025-01-01T00:00:00Z"
            , asSatellites = Map.fromList [("sat1", SatelliteOk checks)]
            , asAllOk = True  -- All OK
            }
      let message = buildAlertMessage status
      message @?= ""
  ]

-- | Test collectFailures
collectFailuresTests :: TestTree
collectFailuresTests = testGroup "collectFailures"
  [ testCase "collects satellite error" $ do
      let satellites = Map.fromList [("sat1", SatelliteError "Connection failed")]
      let failures = collectFailures satellites
      failures @?= ["sat1: Connection failed"]

  , testCase "collects failed health check" $ do
      let checks = Map.fromList [("check1", HealthResult StatusError "Failed" (object []))]
      let satellites = Map.fromList [("sat1", SatelliteOk checks)]
      let failures = collectFailures satellites
      failures @?= ["sat1/check1: Failed"]

  , testCase "ignores ok health checks" $ do
      let checks = Map.fromList
            [ ("check1", HealthResult StatusOk "OK" (object []))
            , ("check2", HealthResult StatusOk "Also OK" (object []))
            ]
      let satellites = Map.fromList [("sat1", SatelliteOk checks)]
      let failures = collectFailures satellites
      failures @?= []

  , testCase "collects only warning and error checks" $ do
      let checks = Map.fromList
            [ ("check1", HealthResult StatusOk "OK" (object []))
            , ("check2", HealthResult StatusWarning "Warning" (object []))
            , ("check3", HealthResult StatusError "Error" (object []))
            ]
      let satellites = Map.fromList [("sat1", SatelliteOk checks)]
      let failures = collectFailures satellites
      length failures @?= 2

  , testCase "handles multiple satellites" $ do
      let checks1 = Map.fromList [("check1", HealthResult StatusError "Error 1" (object []))]
      let checks2 = Map.fromList [("check2", HealthResult StatusError "Error 2" (object []))]
      let satellites = Map.fromList
            [ ("sat1", SatelliteOk checks1)
            , ("sat2", SatelliteOk checks2)
            ]
      let failures = collectFailures satellites
      length failures @?= 2

  , testCase "handles mix of satellite errors and check failures" $ do
      let checks = Map.fromList [("check1", HealthResult StatusError "Check failed" (object []))]
      let satellites = Map.fromList
            [ ("sat1", SatelliteError "Satellite error")
            , ("sat2", SatelliteOk checks)
            ]
      let failures = collectFailures satellites
      length failures @?= 2
      elem "sat1: Satellite error" failures @?= True
      elem "sat2/check1: Check failed" failures @?= True

  , testCase "handles empty satellites" $ do
      let satellites = Map.empty
      let failures = collectFailures satellites
      failures @?= []

  , testCase "handles satellite with no failed checks" $ do
      let checks = Map.fromList [("check1", HealthResult StatusOk "OK" (object []))]
      let satellites = Map.fromList [("sat1", SatelliteOk checks)]
      let failures = collectFailures satellites
      failures @?= []
  ]
