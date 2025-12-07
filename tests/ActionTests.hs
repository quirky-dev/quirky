module ActionTests (tests) where

import Data.Aeson (Value(..), object, (.=))
import Quirky.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Action Execution Tests"
  [ healthResultTests
  , actionErrorTests
  ]

-- | Test HealthResult serialization and validation
healthResultTests :: TestTree
healthResultTests = testGroup "HealthResult"
  [ testCase "creates valid HealthResult with StatusOk" $ do
      let result = HealthResult StatusOk "Everything is fine" (object [])
      hrStatus result @?= StatusOk
      hrMessage result @?= "Everything is fine"

  , testCase "creates valid HealthResult with StatusError" $ do
      let result = HealthResult StatusError "Something went wrong" (object ["error_code" .= Number 500])
      hrStatus result @?= StatusError
      hrMessage result @?= "Something went wrong"

  , testCase "includes arbitrary data in result" $ do
      let testData = object ["count" .= Number 42, "name" .= String "test"]
      let result = HealthResult StatusWarning "Warning message" testData
      hrData result @?= testData
  ]

-- | Test ActionError types
actionErrorTests :: TestTree
actionErrorTests = testGroup "ActionError"
  [ testCase "creates ExecutionError" $ do
      let err = ExecutionError "Failed to execute"
      err @?= ExecutionError "Failed to execute"

  , testCase "creates TemplatingError" $ do
      let err = TemplatingError "Invalid template"
      err @?= TemplatingError "Invalid template"

  , testCase "creates ConfigError" $ do
      let err = ConfigError "Missing required field"
      err @?= ConfigError "Missing required field"

  , testCase "creates TimeoutError" $ do
      let err = TimeoutError
      err @?= TimeoutError

  , testCase "errors are comparable" $ do
      ExecutionError "test" @?= ExecutionError "test"
      assertBool "Different error types should not be equal"
        (TemplatingError "test" /= ExecutionError "test")
  ]
