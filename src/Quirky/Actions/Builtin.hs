module Quirky.Actions.Builtin where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import System.Directory (doesFileExist, getModificationTime, getFileSize)
import System.Environment (lookupEnv)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Base64 as B64
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Text.IO as TIO
import Data.Scientific (toRealFloat)
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (hAuthorization)
import qualified Data.CaseInsensitive as CI
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Quirky.Types

-- | Execute a builtin action
executeBuiltinAction :: BuiltinAction -> ActionConfig -> ExceptT ActionError IO Value
executeBuiltinAction action config = case action of
  HttpGet -> httpGetAction config
  ParseJson -> parseJsonAction config
  ParseHtml -> parseHtmlAction config
  RegexMatch -> regexMatchAction config
  JsonExtract -> jsonExtractAction config
  SqlQuery -> sqlQueryAction config
  SshCommand -> sshCommandAction config
  FileStat -> fileStatAction config
  CompareNumber -> compareNumberAction config
  CompareTimestamp -> compareTimestampAction config

-- | HTTP GET action
httpGetAction :: ActionConfig -> ExceptT ActionError IO Value
httpGetAction config = do
  -- Parse config
  url <- extractTextField "url" config

  -- Check for auth config
  authConfig <- case config of
    Object obj -> case KM.lookup "auth" obj of
      Just authObj -> return $ Just authObj
      Nothing -> return Nothing
    _ -> return Nothing

  -- Make HTTP request
  manager <- liftIO $ newManager tlsManagerSettings
  baseRequest <- liftIO $ parseRequest (T.unpack url)

  -- Apply authentication if configured
  request <- case authConfig of
    Just auth -> applyAuth auth baseRequest
    Nothing -> return baseRequest

  response <- liftIO $ httpLbs request manager

  -- Return response as JSON
  return $ object
    [ "status_code" .= (statusCode $ responseStatus response)
    , "body" .= decodeUtf8 (responseBody response)
    ]
  where
    decodeUtf8 = Data.Text.Encoding.decodeUtf8 . toStrict
    toStrict = Data.ByteString.Lazy.toStrict

    applyAuth :: Value -> Request -> ExceptT ActionError IO Request
    applyAuth authVal req = do
      authType <- extractTextField "type" authVal
      case authType of
        "basic" -> do
          username <- resolveCredential "username" authVal
          password <- resolveCredential "password" authVal
          case (username, password) of
            (Just u, Just p) -> do
              let credentials = Data.Text.Encoding.encodeUtf8 $ u <> ":" <> p
              let encoded = B64.encode credentials
              return $ req { requestHeaders = (hAuthorization, "Basic " <> encoded) : requestHeaders req }
            _ -> throwError $ ConfigError "Basic auth requires username and password"

        "header" -> do
          headerName <- extractTextField "header" authVal
          headerValue <- resolveCredential "value" authVal
          case headerValue of
            Just val -> do
              let headerName' = CI.mk $ Data.Text.Encoding.encodeUtf8 headerName
              let valueBytes = Data.Text.Encoding.encodeUtf8 val
              return $ req { requestHeaders = (headerName', valueBytes) : requestHeaders req }
            Nothing -> throwError $ ConfigError "Header auth requires value"

        "query" -> do
          paramName <- extractTextField "param" authVal
          paramValue <- resolveCredential "value" authVal
          case paramValue of
            Just val -> do
              let currentQuery = queryString req
              let newParam = Data.Text.Encoding.encodeUtf8 $ paramName <> "=" <> val
              let newQuery = if BS.null currentQuery
                            then "?" <> newParam
                            else currentQuery <> "&" <> newParam
              return $ req { queryString = newQuery }
            Nothing -> throwError $ ConfigError "Query auth requires value"

        _ -> throwError $ ConfigError $ "Unknown auth type: " <> authType

-- | Parse JSON action
parseJsonAction :: ActionConfig -> ExceptT ActionError IO Value
parseJsonAction config = do
  input <- extractTextField "input" config

  case decode (Data.ByteString.Lazy.fromStrict $ Data.Text.Encoding.encodeUtf8 input) :: Maybe Value of
    Nothing -> throwError $ ExecutionError "Failed to parse JSON"
    Just val -> return $ object
      [ "status" .= ("ok" :: Text)
      , "message" .= ("Successfully parsed JSON" :: Text)
      , "data" .= (val :: Value)
      ]

-- | Extract value from JSON using a path (e.g., "channel.item[0].pubDate")
-- Note: This action expects the JSON to be passed directly as a Value in the config,
-- not as a templated string. Use json_ref in the config to reference a context variable.
jsonExtractAction :: ActionConfig -> ExceptT ActionError IO Value
jsonExtractAction config = do
  -- Get the JSON input - it should already be a Value in the config
  jsonInput <- case config of
    Object obj -> case KM.lookup "json" obj of
      Just val -> return val
      Nothing -> throwError $ ConfigError "Missing 'json' or 'json_ref' field"
    _ -> throwError $ ConfigError "Config is not an object"

  -- Get the path to extract
  path <- extractTextField "path" config

  -- Navigate the path
  case extractJsonPath (T.splitOn "." path) jsonInput of
    Nothing -> return $ object
      [ "status" .= ("error" :: Text)
      , "message" .= ("Path not found: " <> path)
      , "data" .= object [ "path" .= path ]
      ]
    Just val -> return $ object
      [ "status" .= ("ok" :: Text)
      , "message" .= ("Extracted value from: " <> path)
      , "data" .= object
          [ "path" .= path
          , "value" .= val
          ]
      ]
  where
    -- Extract a value from JSON using a path like ["channel", "item[0]", "pubDate"]
    extractJsonPath :: [Text] -> Value -> Maybe Value
    extractJsonPath [] val = Just val
    extractJsonPath (key:rest) (Object obj) =
      -- Check if key has array index like "item[0]"
      case T.breakOn "[" key of
        (fieldName, indexPart)
          | T.null indexPart -> do
              -- No array index, just field lookup
              val <- KM.lookup (K.fromText key) obj
              extractJsonPath rest val
          | otherwise -> do
              -- Has array index like "item[0]"
              val <- KM.lookup (K.fromText fieldName) obj
              case val of
                Array arr -> do
                  -- Extract index from "[0]"
                  let indexStr = T.drop 1 $ T.takeWhile (/= ']') indexPart
                  case reads (T.unpack indexStr) of
                    [(idx, "")] | idx >= 0 && idx < V.length arr -> do
                      extractJsonPath rest (arr V.! idx)
                    _ -> Nothing
                _ -> Nothing
    extractJsonPath _ _ = Nothing

-- | Parse HTML action (extract text from elements)
parseHtmlAction :: ActionConfig -> ExceptT ActionError IO Value
parseHtmlAction config = do
  html <- extractTextField "html" config
  selector <- extractTextField "selector" config

  -- Parse HTML
  let tags = parseTags html

  -- Simple selector matching (tag name or class)
  let results = case T.unpack selector of
        -- Class selector: .classname
        ('.':className) -> extractByClass tags (T.pack className)
        -- Tag selector: tagname
        tagName -> extractByTag tags (T.pack tagName)

  -- Return results
  return $ object
    [ "matches" .= length results
    , "results" .= results
    , "selector" .= selector
    ]
  where
    extractByTag tags tagName =
      [extractInnerText section | section <- sectionsMatching tags (TagOpen tagName [])]

    extractByClass tags className =
      let matchTag = TagOpen "" [("class", className)]
      in [extractInnerText section | section <- sectionsMatching tags matchTag]

    -- Get all sections between opening and closing tags
    sectionsMatching :: [Tag Text] -> Tag Text -> [[Tag Text]]
    sectionsMatching tags matchTag = go tags
      where
        go [] = []
        go (t:ts)
          | t ~== matchTag = let (sect, rest) = break isClosing ts
                             in sect : go rest
          | otherwise = go ts

        isClosing (TagClose _) = True
        isClosing _ = False

    -- Extract inner text from a section of tags
    extractInnerText :: [Tag Text] -> Text
    extractInnerText tags = T.strip $ T.concat [txt | TagText txt <- tags]

-- | Regex match action
regexMatchAction :: ActionConfig -> ExceptT ActionError IO Value
regexMatchAction config = do
  input <- extractTextField "input" config
  pattern <- extractTextField "pattern" config

  -- Optional: capture group index (default 0 = full match)
  captureGroup <- case config of
    Object obj -> case KM.lookup "capture_group" obj of
      Just (Number n) -> return $ round (toRealFloat n :: Double)
      Just _ -> throwError $ ConfigError "capture_group must be a number"
      Nothing -> return 0
    _ -> return 0

  -- Perform regex match using Text
  let matches = input =~ pattern :: [[Text]]

  case matches of
    [] ->
      -- No match - return error status for health check
      return $ object
        [ "status" .= ("error" :: Text)
        , "message" .= ("Pattern not found: " <> pattern)
        , "data" .= object
            [ "matched" .= False
            , "pattern" .= pattern
            ]
        ]
    (firstMatch:_) -> do
      -- Extract the requested capture group
      let captureValue = if captureGroup < length firstMatch
                         then firstMatch !! captureGroup
                         else ""

      return $ object
        [ "status" .= ("ok" :: Text)
        , "message" .= ("Pattern matched: " <> pattern)
        , "data" .= object
            [ "matched" .= True
            , "pattern" .= pattern
            , "match" .= captureValue
            , "all_captures" .= firstMatch
            ]
        ]

-- | SQL query action
sqlQueryAction :: ActionConfig -> ExceptT ActionError IO Value
sqlQueryAction _config = do
  -- TODO: Implement SQL query execution
  throwError $ ExecutionError "SQL queries not yet implemented"

-- | SSH command action
sshCommandAction :: ActionConfig -> ExceptT ActionError IO Value
sshCommandAction _config = do
  -- TODO: Implement SSH command execution
  throwError $ ExecutionError "SSH commands not yet implemented"

-- | File stat action
fileStatAction :: ActionConfig -> ExceptT ActionError IO Value
fileStatAction config = do
  path <- extractTextField "path" config

  exists <- liftIO $ doesFileExist (T.unpack path)

  if not exists
    then return $ object
      [ "exists" .= False
      , "path" .= path
      ]
    else do
      modTime <- liftIO $ getModificationTime (T.unpack path)
      size <- liftIO $ getFileSize (T.unpack path)

      return $ object
        [ "exists" .= True
        , "path" .= path
        , "modified" .= show modTime
        , "size" .= size
        ]

-- | Compare number action (for thresholds)
compareNumberAction :: ActionConfig -> ExceptT ActionError IO Value
compareNumberAction config = do
  value <- extractNumberField "value" config
  threshold <- extractNumberField "threshold" config
  operator <- extractTextField "operator" config

  let result = case operator of
        ">" -> value > threshold
        ">=" -> value >= threshold
        "<" -> value < threshold
        "<=" -> value <= threshold
        "==" -> value == threshold
        "!=" -> value /= threshold
        _ -> False

  let status = if result then "ok" else "error" :: Text
  let message = if result
        then T.pack $ show value ++ " " ++ T.unpack operator ++ " " ++ show threshold
        else T.pack $ show value ++ " not " ++ T.unpack operator ++ " " ++ show threshold

  return $ object
    [ "status" .= (status :: Text)
    , "message" .= (message :: Text)
    , "data" .= object
        [ "result" .= result
        , "value" .= value
        , "threshold" .= threshold
        , "operator" .= operator
        ]
    ]

-- | Compare timestamp action (for age checks)
compareTimestampAction :: ActionConfig -> ExceptT ActionError IO Value
compareTimestampAction config = do
  timestampText <- extractTextField "timestamp" config
  maxAgeHours <- extractNumberField "max_age_hours" config

  -- Parse the ISO 8601 timestamp
  case parseISO8601 (T.unpack timestampText) of
    Nothing -> throwError $ ExecutionError $ "Invalid timestamp format: " <> timestampText
    Just timestamp -> do
      -- Get current time
      now <- liftIO getCurrentTime

      -- Calculate age in hours
      let age = diffUTCTime now timestamp
      let ageHours = realToFrac age / 3600 :: Double

      -- Check if timestamp is within acceptable age
      let result = ageHours <= maxAgeHours

      let status = if result then "ok" else "error" :: Text
      let message = if result
            then T.pack $ "Timestamp is " ++ show (round ageHours :: Int) ++ " hours old (max: " ++ show (round maxAgeHours :: Int) ++ ")"
            else T.pack $ "Timestamp is too old: " ++ show (round ageHours :: Int) ++ " hours (max: " ++ show (round maxAgeHours :: Int) ++ ")"

      return $ object
        [ "status" .= (status :: Text)
        , "message" .= (message :: Text)
        , "data" .= object
            [ "timestamp" .= timestampText
            , "age_hours" .= ageHours
            , "max_age_hours" .= maxAgeHours
            , "current_time" .= show now
            ]
        ]

-- Parse ISO 8601 timestamp (simple implementation)
parseISO8601 :: String -> Maybe UTCTime
parseISO8601 s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" s

-- Helper functions

extractTextField :: Text -> ActionConfig -> ExceptT ActionError IO Text
extractTextField field config = case config of
  Object obj -> case KM.lookup (K.fromText field) obj of
    Just (String txt) -> return txt
    Just _ -> throwError $ ConfigError $ "Field " <> field <> " is not a string"
    Nothing -> throwError $ ConfigError $ "Missing field: " <> field
  _ -> throwError $ ConfigError "Config is not an object"

extractOptionalTextField :: Text -> ActionConfig -> ExceptT ActionError IO (Maybe Text)
extractOptionalTextField field config = case config of
  Object obj -> case KM.lookup (K.fromText field) obj of
    Just (String txt) -> return $ Just txt
    Just _ -> throwError $ ConfigError $ "Field " <> field <> " is not a string"
    Nothing -> return Nothing
  _ -> throwError $ ConfigError "Config is not an object"

extractNumberField :: Text -> ActionConfig -> ExceptT ActionError IO Double
extractNumberField field config = case config of
  Object obj -> case KM.lookup (K.fromText field) obj of
    Just (Number n) -> return $ toRealFloat n
    Just (String s) -> case reads (T.unpack s) of
      [(n, "")] -> return n
      _ -> throwError $ ConfigError $ "Field " <> field <> " is not a valid number: " <> s
    Just _ -> throwError $ ConfigError $ "Field " <> field <> " is not a number"
    Nothing -> throwError $ ConfigError $ "Missing field: " <> field
  _ -> throwError $ ConfigError "Config is not an object"

-- | Resolve a credential value from file, env, or inline
-- Priority: {field}_file > {field}_env > {field}
resolveCredential :: Text -> ActionConfig -> ExceptT ActionError IO (Maybe Text)
resolveCredential field config = do
  fileValue <- extractOptionalTextField (field <> "_file") config
  case fileValue of
    Just filePath -> do
      -- Read from file
      content <- liftIO $ TIO.readFile (T.unpack filePath)
      return $ Just $ T.strip content
    Nothing -> do
      envValue <- extractOptionalTextField (field <> "_env") config
      case envValue of
        Just envVar -> do
          -- Read from environment variable
          maybeVal <- liftIO $ lookupEnv (T.unpack envVar)
          case maybeVal of
            Just val -> return $ Just $ T.pack val
            Nothing -> throwError $ ConfigError $ "Environment variable not found: " <> envVar
        Nothing -> do
          -- Try inline value
          extractOptionalTextField field config
