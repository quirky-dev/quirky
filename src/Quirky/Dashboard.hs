module Quirky.Dashboard where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Quirky.Types

-- | Generate HTML dashboard
generateDashboard :: AggregatedStatus -> Text
generateDashboard status = T.unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , "  <meta charset='utf-8'>"
  , "  <meta name='viewport' content='width=device-width, initial-scale=1'>"
  , "  <title>Quirky Status</title>"
  , "  <style>"
  , dashboardCSS
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <div class='container'>"
  , "    <h1>Quirky Status Dashboard</h1>"
  , "    <div class='timestamp'>Last updated: <span id='timestamp' data-unix='" <> asTimestamp status <> "'></span></div>"
  , "    <div class='satellites'>"
  , T.concat $ map renderSatellite (Map.toList $ asSatellites status)
  , "    </div>"
  , "  </div>"
  , "  <script>"
  , "    // Format timestamp in user's local time"
  , "    const timestampEl = document.getElementById('timestamp');"
  , "    const unixTime = parseInt(timestampEl.dataset.unix);"
  , "    const date = new Date(unixTime * 1000);"
  , "    const options = {"
  , "      year: 'numeric', month: 'short', day: 'numeric',"
  , "      hour: '2-digit', minute: '2-digit', second: '2-digit'"
  , "    };"
  , "    timestampEl.textContent = date.toLocaleString(undefined, options);"
  , ""
  , "    // Auto-refresh every 30 seconds"
  , "    setTimeout(() => location.reload(), 30000);"
  , "  </script>"
  , "</body>"
  , "</html>"
  ]

-- | Render a single satellite section
renderSatellite :: (Text, SatelliteStatus) -> Text
renderSatellite (name, SatelliteError err) = T.unlines
  [ "<div class='satellite'>"
  , "  <h2>" <> name <> "</h2>"
  , "  <div class='check error'>"
  , "    <span class='status'>ERROR</span>"
  , "    <span class='message'>" <> err <> "</span>"
  , "  </div>"
  , "</div>"
  ]

renderSatellite (name, SatelliteOk checks) = T.unlines
  [ "<div class='satellite'>"
  , "  <h2>" <> name <> "</h2>"
  , T.concat $ map renderCheck (Map.toList checks)
  , "</div>"
  ]

-- | Render a single check
renderCheck :: (Text, HealthResult) -> Text
renderCheck (checkName, result) =
  let statusClass = case hrStatus result of
        StatusOk -> "ok"
        StatusWarning -> "warning"
        StatusError -> "error"
      statusText = case hrStatus result of
        StatusOk -> "OK"
        StatusWarning -> "WARN"
        StatusError -> "ERROR"
  in T.unlines
    [ "<div class='check " <> statusClass <> "'>"
    , "  <span class='check-name'>" <> checkName <> "</span>"
    , "  <span class='status'>" <> statusText <> "</span>"
    , "  <span class='message'>" <> hrMessage result <> "</span>"
    , "</div>"
    ]

-- | CSS for the dashboard
dashboardCSS :: Text
dashboardCSS = T.unlines
  [ "* { margin: 0; padding: 0; box-sizing: border-box; }"
  , "body {"
  , "  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;"
  , "  background: #0f0f0f;"
  , "  color: #e0e0e0;"
  , "  padding: 20px;"
  , "}"
  , ".container { max-width: 1200px; margin: 0 auto; }"
  , "h1 {"
  , "  font-size: 2em;"
  , "  margin-bottom: 10px;"
  , "  color: #fff;"
  , "}"
  , ".timestamp {"
  , "  color: #888;"
  , "  margin-bottom: 30px;"
  , "  font-size: 0.9em;"
  , "}"
  , ".satellite {"
  , "  background: #1a1a1a;"
  , "  border-radius: 8px;"
  , "  padding: 20px;"
  , "  margin-bottom: 20px;"
  , "  border-left: 4px solid #333;"
  , "}"
  , ".satellite h2 {"
  , "  font-size: 1.3em;"
  , "  margin-bottom: 15px;"
  , "  color: #fff;"
  , "  text-transform: uppercase;"
  , "  letter-spacing: 1px;"
  , "}"
  , ".check {"
  , "  display: grid;"
  , "  grid-template-columns: 2fr 80px 3fr;"
  , "  gap: 15px;"
  , "  padding: 12px 15px;"
  , "  background: #222;"
  , "  border-radius: 4px;"
  , "  margin-bottom: 8px;"
  , "  align-items: center;"
  , "  border-left: 3px solid transparent;"
  , "}"
  , ".check.ok { border-left-color: #22c55e; }"
  , ".check.warning { border-left-color: #eab308; }"
  , ".check.error { border-left-color: #ef4444; }"
  , ".check-name {"
  , "  font-weight: 500;"
  , "  color: #e0e0e0;"
  , "}"
  , ".status {"
  , "  font-weight: 700;"
  , "  font-size: 0.85em;"
  , "  padding: 4px 8px;"
  , "  border-radius: 3px;"
  , "  text-align: center;"
  , "}"
  , ".check.ok .status {"
  , "  background: #16a34a;"
  , "  color: #fff;"
  , "}"
  , ".check.warning .status {"
  , "  background: #ca8a04;"
  , "  color: #fff;"
  , "}"
  , ".check.error .status {"
  , "  background: #dc2626;"
  , "  color: #fff;"
  , "}"
  , ".message {"
  , "  color: #aaa;"
  , "  font-size: 0.9em;"
  , "}"
  ]
