# Quirky - Simple, Flexible Health Monitoring

A lightweight health monitoring system that lets you write checks in **any language**.

## Philosophy

Most monitoring systems are either:
- Too complex (Prometheus/Grafana requires learning PromQL, setting up storage, etc.)
- Too rigid (can't easily add custom checks)
- Too opinionated (forces specific metric formats)

Quirky is different:
- **Simple**: One binary, one config file
- **Flexible**: Write checks in bash, Python, Node.js, or anything that outputs JSON
- **Quirky**: Built-in checks for unusual patterns (e.g., "last database insert age", "homepage scraping")

## Quick Start

```bash
# Run a satellite (monitors local system, exposes /health endpoint)
quirky --config examples/satellite.yaml

# Run an aggregator (polls satellites, shows dashboard, sends alerts)
quirky --config examples/aggregator.yaml

# View the dashboard
# Satellite: http://localhost:9855
# Aggregator: http://localhost:8080
```

## How Actions Work

Quirky supports three types of actions:

### 1. Builtin Actions (no slash)
Built-in actions for common tasks:

```yaml
actions:
  - name: fetch
    action: "http_get"
    config:
      url: "https://api.example.com/health"

  - name: check_status
    action: "compare_number"
    config:
      value: "{{ fetch.status_code }}"
      equals: 200
```

**Available builtins**: `http_get`, `parse_json`, `parse_html`, `regex_match`, `json_extract`, `sql_query`, `ssh_command`, `file_stat`, `compare_number`, `compare_timestamp`

### 2. Bundled Actions (with slash)
Pre-built scripts in the `actions/` directory, referenced by category/name:

```yaml
actions:
  - action: "system/check-load"
    config:
      threshold_multiplier: 2.0

  - action: "database/postgres-connection"
    config:
      connection_string: "postgresql://user@localhost/db"
```

**System Monitoring:**
- `system/check-load` - CPU load average
- `system/check-disk` - Disk usage
- `system/check-memory` - Memory usage

**Database Checks:**
- `database/postgres-connection` - PostgreSQL connection test
- `database/postgres-query-age` - Time since last query
- `database/postgres-replication` - Replication status

**Backup Checks:**
- `backup/pgbackrest-status` - pgBackRest backup status
- `backup/ssh-file-age` - Remote file age via SSH

Bundled actions are resolved by searching in:
- `~/.quirky/actions/` (user custom actions)
- `./actions/` (local to working directory)
- Bundled with Quirky binary
- System-wide locations (`/usr/share/quirky/actions/`, etc.)

### 3. Custom Scripts (explicit path)
Your own scripts referenced with explicit paths:

```yaml
actions:
  - action: "./checks/my-custom-check.sh"
    config:
      threshold: 100
```

## Writing Custom Checks

Any executable can be a check. It must:
1. Read JSON configuration from stdin
2. Write JSON result to stdout
3. Exit with code 0

**Output format:**
```json
{
  "status": "ok|warning|error",
  "message": "Human-readable status",
  "data": {
    "any": "additional data"
  }
}
```

### Bash Example

```bash
#!/usr/bin/env bash
set -euo pipefail

CONFIG=$(cat)
THRESHOLD=$(echo "$CONFIG" | jq -r '.threshold // 90')
PATH=$(echo "$CONFIG" | jq -r '.path // "/"')

USAGE=$(df -h "$PATH" | tail -1 | awk '{print $5}' | sed 's/%//')

if [ "$USAGE" -lt "$THRESHOLD" ]; then
  STATUS="ok"
else
  STATUS="error"
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "Disk usage: ${USAGE}%" \
  --argjson usage "$USAGE" \
  '{status: $status, message: $message, data: {usage_percent: $usage}}'
```

### Python Example

```python
#!/usr/bin/env python3
import json
import sys

config = json.load(sys.stdin)
threshold = config.get('threshold', 90)

# Do your check here...
current_value = 75

if current_value < threshold:
    status = "ok"
else:
    status = "error"

result = {
    "status": status,
    "message": f"Value: {current_value}",
    "data": {"value": current_value}
}

print(json.dumps(result))
```

### Node.js Example

```javascript
#!/usr/bin/env node

let config = '';
process.stdin.on('data', chunk => config += chunk);
process.stdin.on('end', () => {
  const params = JSON.parse(config);
  const threshold = params.threshold || 90;

  // Do your check here...
  const currentValue = 75;

  const result = {
    status: currentValue < threshold ? 'ok' : 'error',
    message: `Value: ${currentValue}`,
    data: { value: currentValue }
  };

  console.log(JSON.stringify(result));
});
```

## Configuration Examples

See the `examples/` directory:

- **satellite.yaml** - Complete satellite config with builtin actions, bundled actions, and custom scripts
- **aggregator.yaml** - Aggregator config for polling satellites and sending alerts
- **advanced-examples.yaml** - Advanced HTTP/parsing examples with authentication

## Architecture

**Satellite Mode**: Runs checks locally, exposes `/health` endpoint
- Lightweight monitoring agent
- Runs on each server you want to monitor
- Can be polled by aggregator or queried directly

**Aggregator Mode**: Polls satellites, aggregates results, sends alerts, serves dashboard
- Central monitoring server
- Polls multiple satellites
- Sends alerts (Pushover, Slack)
- Provides web dashboard

## Features

- ✅ **Simple**: One binary, YAML config
- ✅ **Flexible**: Write checks in any language
- ✅ **Builtin Actions**: HTTP, SQL, SSH, file stats, comparisons
- ✅ **Templating**: Reference previous action results
- ✅ **Alerts**: Pushover, Slack
- ✅ **Web Dashboard**: View all checks at a glance
- ✅ **Action Library**: Pre-built checks for common tasks

## License

MIT
