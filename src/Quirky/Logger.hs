module Quirky.Logger
  ( withLogging
  ) where

import Control.Monad.Logger
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Console.ANSI
import System.Environment (lookupEnv)
import System.IO (stdout, stderr, hFlush, hPutStr, hPutStrLn, Handle)
import System.Log.FastLogger (fromLogStr)

-- | Run logging with colored output (default level: INFO)
withLogging :: LoggingT IO a -> IO a
withLogging action = do
  -- Check LOG_LEVEL environment variable, default to INFO
  minLevel <- maybe LevelInfo parseLevel <$> lookupEnv "LOG_LEVEL"
  runLoggingT action (coloredOutput minLevel)
  where
    parseLevel "DEBUG" = LevelDebug
    parseLevel "INFO" = LevelInfo
    parseLevel "WARN" = LevelWarn
    parseLevel "ERROR" = LevelError
    parseLevel _ = LevelInfo

    coloredOutput :: LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    coloredOutput minLevel _loc _source level msg = do
      -- Skip messages below minimum level
      if level < minLevel
        then return ()
        else do
          let (color, levelStr) = case level of
                LevelDebug -> (Cyan, "DEBUG")
                LevelInfo  -> (Green, "INFO ")
                LevelWarn  -> (Yellow, "WARN ")
                LevelError -> (Red, "ERROR")
                LevelOther t -> (Magenta, T.unpack t)

              -- Use stderr for errors, stdout for everything else
              handle = if level >= LevelError then stderr else stdout

          -- Print colored level tag (aligned to 5 chars)
          hSetSGR handle [SetColor Foreground Vivid color, SetConsoleIntensity BoldIntensity]
          hPutStr handle $ "[" ++ levelStr ++ "]"
          hSetSGR handle [Reset]

          -- Print message
          hPutStr handle $ " " ++ T.unpack (T.decodeUtf8 $ fromLogStr msg)
          hPutStrLn handle ""
          hFlush handle
