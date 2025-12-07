module Quirky.Engine where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Quirky.Types
import Quirky.Actions.Builtin
import Quirky.Actions.Resolver
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- | Execute a single health check
executeCheck :: HealthCheck -> IO (Either ActionError HealthResult)
executeCheck hc = runExceptT $ do
  -- Execute actions sequentially, building up context
  finalContext <- foldM executeStep Map.empty (hcActions hc)

  -- Get the result from the last action
  -- If the last action had a name, use it; otherwise use "_last"
  case hcActions hc of
    [] -> throwError $ ExecutionError "No actions in health check"
    actions -> do
      let lastActionKey = maybe "_last" id (asName (last actions))
      case Map.lookup lastActionKey finalContext of
        Just (Object obj) -> do
          status <- case KM.lookup (K.fromText "status") obj of
            Just (String "ok") -> pure StatusOk
            Just (String "warning") -> pure StatusWarning
            Just (String "error") -> pure StatusError
            _ -> throwError $ ExecutionError "Invalid status in result"

          message <- case KM.lookup (K.fromText "message") obj of
            Just (String msg) -> pure msg
            _ -> throwError $ ExecutionError "Missing message in result"

          let resultData = maybe (object []) id $ KM.lookup (K.fromText "data") obj

          return $ HealthResult status message resultData

        Nothing -> throwError $ ExecutionError $ "No result found for action with key: " <> lastActionKey
        Just _ -> throwError $ ExecutionError "Result is not an object"

-- | Execute a single action step, updating the context
executeStep :: ActionContext -> ActionStep -> ExceptT ActionError IO ActionContext
executeStep ctx step = do
  -- Apply templating to config (use empty object if no config provided)
  let stepConfig = maybe (object []) id (asConfig step)
  templatedConfig <- liftEither $ applyTemplating ctx stepConfig

  -- Execute the action
  result <- case asAction step of
    BuiltinAction builtin -> executeBuiltinAction builtin templatedConfig

    NamedAction name -> do
      -- Resolve named action to executable path
      resolved <- liftIO $ resolveAction name
      case resolved of
        Right path -> executeExternalAction (T.pack path) templatedConfig
        Left (ActionNotFound actionName searchedPaths) -> do
          let errMsg = "Action not found: " <> actionName <> "\nSearched in: " <> T.pack (show searchedPaths)
          liftIO $ runStdoutLoggingT $ $logError $ "Action resolution failed: " <> errMsg
          throwError $ ExecutionError errMsg
        Left (InvalidActionName actionName) -> do
          let errMsg = "Invalid action name: " <> actionName
          liftIO $ runStdoutLoggingT $ $logError $ "Action resolution failed: " <> errMsg
          throwError $ ExecutionError errMsg

    CompositeAction steps -> do
      -- Execute sub-steps and return final context
      _ <- foldM executeStep ctx steps
      -- For composite actions, we need to return the step result
      -- This is a simplification - in practice we'd need better handling
      throwError $ ExecutionError "Composite actions not yet fully implemented"

  -- Add result to context with the step name (or "_last" if no name)
  let key = maybe "_last" id (asName step)

  -- For easier templating, if result has a "data" field, merge it into the top level
  let flattenedResult = case result of
        Object obj -> case KM.lookup "data" obj of
          Just (Object dataObj) -> Object $ KM.union obj dataObj  -- Merge data fields into top level
          _ -> result
        _ -> result

  return $ Map.insert key flattenedResult ctx

-- | Apply template substitutions like {{ foo.bar }} to a JSON value
-- Special handling: if an object has a "json_ref" field, replace "json" with the referenced value
applyTemplating :: ActionContext -> Value -> Either ActionError Value
applyTemplating ctx val = case val of
  String txt -> Right $ String $ replaceTemplates ctx txt
  Array arr -> Array <$> mapM (applyTemplating ctx) arr
  Object obj -> do
    -- Check for json_ref field for direct value injection
    case KM.lookup "json_ref" obj of
      Just (String refPath) -> do
        -- Look up the referenced value and inject it as "json"
        case lookupContextPath refPath ctx of
          Just refVal -> do
            -- Remove json_ref and add json with the actual value
            let obj' = KM.delete "json_ref" $ KM.insert "json" refVal obj
            Object <$> mapM (applyTemplating ctx) obj'
          Nothing -> Left $ TemplatingError $ "json_ref not found: " <> refPath
      _ -> Object <$> mapM (applyTemplating ctx) obj
  other -> Right other
  where
    lookupContextPath :: Text -> ActionContext -> Maybe Value
    lookupContextPath path ctx' = case T.splitOn "." path of
      [] -> Nothing
      (key:rest) -> case Map.lookup key ctx' of
        Nothing -> Nothing
        Just val' -> followPath rest val'

    followPath :: [Text] -> Value -> Maybe Value
    followPath [] v = Just v
    followPath (key:rest) (Object o) =
      case KM.lookup (K.fromText key) o of
        Nothing -> Nothing
        Just v -> followPath rest v
    followPath _ _ = Nothing

-- | Replace {{ template }} references in text
replaceTemplates :: ActionContext -> Text -> Text
replaceTemplates ctx txt = go txt
  where
    go t = case T.breakOn "{{" t of
      (before, rest)
        | T.null rest -> before  -- No more templates
        | otherwise -> case T.breakOn "}}" (T.drop 2 rest) of
            (_, afterClose)
              | T.null afterClose -> t  -- Malformed template, return as-is
            (varPath, afterClose) ->
              let cleaned = T.strip varPath
                  value = lookupPath cleaned ctx
                  replacement = case value of
                    Just (String s) -> s
                    Just (Number n) -> T.pack $ show n
                    Just (Bool b) -> if b then "true" else "false"
                    Just Null -> ""
                    Just other -> T.decodeUtf8 $ BL.toStrict $ encode other
                    Nothing -> "{{" <> cleaned <> "}}"  -- Keep original if not found
              in before <> replacement <> go (T.drop 2 afterClose)

    -- Look up a path like "foo.bar" in the context
    lookupPath :: Text -> ActionContext -> Maybe Value
    lookupPath path ctxMap = case T.splitOn "." path of
      [] -> Nothing
      (key:rest) -> case Map.lookup key ctxMap of
        Nothing -> Nothing
        Just val -> followPath rest val

    -- Follow a path through nested JSON objects
    followPath :: [Text] -> Value -> Maybe Value
    followPath [] val = Just val
    followPath (key:rest) (Object obj) =
      case KM.lookup (K.fromText key) obj of
        Nothing -> Nothing
        Just val -> followPath rest val
    followPath _ _ = Nothing

-- | Execute an external action (executable)
executeExternalAction :: Text -> ActionConfig -> ExceptT ActionError IO Value
executeExternalAction path config = do
  -- Execute the external process with config as stdin
  let configJson = T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode config
  (exitCode, stdout, stderrOut) <- liftIO $ readProcessWithExitCode (T.unpack path) [] configJson

  case exitCode of
    ExitSuccess -> do
      -- Parse stdout as JSON
      case decode (BL.fromStrict $ T.encodeUtf8 $ T.pack stdout) of
        Just result -> return result
        Nothing -> do
          let errMsg = "Failed to parse output as JSON: " <> T.pack stdout
          liftIO $ runStdoutLoggingT $ $logError $ "Action error [" <> path <> "]: " <> errMsg
          throwError $ ExecutionError errMsg
    ExitFailure code -> do
      let errMsg = "Process exited with code " <> T.pack (show code) <> ": " <> T.pack stderrOut
      liftIO $ runStdoutLoggingT $ do
        $logError $ "Action failed [" <> path <> "]: exit code " <> T.pack (show code)
        $logError $ "  stderr: " <> T.pack stderrOut
        $logError $ "  stdout: " <> T.pack stdout
      throwError $ ExecutionError errMsg
