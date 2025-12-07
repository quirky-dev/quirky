module Quirky.Actions.Resolver
  ( resolveAction
  , getActionSearchPaths
  , ActionResolutionError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, getHomeDirectory, makeAbsolute)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory, (<.>))

data ActionResolutionError
  = ActionNotFound Text [FilePath]  -- Action name and searched paths
  | InvalidActionName Text
  deriving (Show, Eq)

-- | Resolve a named action to an executable path
-- Searches in order:
--   1. ~/.quirky/actions/
--   2. ./actions/ (relative to CWD)
--   3. [executable-dir]/actions/ (bundled with binary)
--   4. /usr/share/quirky/actions/ (system-wide)
--   5. /usr/local/share/quirky/actions/ (local install)
resolveAction :: Text -> IO (Either ActionResolutionError FilePath)
resolveAction name = do
  paths <- getActionSearchPaths
  result <- findInPaths name paths
  case result of
    Just path -> pure $ Right path
    Nothing -> pure $ Left $ ActionNotFound name paths

-- | Find action script in search paths
findInPaths :: Text -> [FilePath] -> IO (Maybe FilePath)
findInPaths _ [] = pure Nothing
findInPaths name (p:ps) = do
  let scriptPath = p </> T.unpack name <.> "sh"
  exists <- doesFileExist scriptPath
  if exists
    then Just <$> makeAbsolute scriptPath
    else findInPaths name ps

-- | Get action search paths in priority order
getActionSearchPaths :: IO [FilePath]
getActionSearchPaths = do
  homeDir <- getHomeDirectory
  exePath <- getExecutablePath
  let exeDir = takeDirectory exePath
  let packageRoot = takeDirectory exeDir  -- Go up from bin/ to package root

  pure
    [ homeDir </> ".quirky" </> "actions"       -- User's custom actions
    , "actions"                                  -- Local to CWD
    , packageRoot </> "share" </> "quirky" </> "actions"  -- Bundled with binary
    , "/usr/share/quirky/actions"                -- System-wide (Linux)
    , "/usr/local/share/quirky/actions"          -- Homebrew/local install
    ]
