#!/usr/bin/env bash
# Action: database/postgres-connection
# Description: Check if PostgreSQL is accepting connections
# Parameters:
#   - database (optional): Specific database to connect to
#   - user (optional, default: postgres): User to connect as

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
DATABASE=$(echo "$CONFIG" | jq -r '.database // ""')
USER=$(echo "$CONFIG" | jq -r '.user // "postgres"')

# Build psql command
PSQL_CMD="sudo -u $USER psql"
if [ -n "$DATABASE" ] && [ "$DATABASE" != "null" ]; then
  PSQL_CMD="$PSQL_CMD -d $DATABASE"
fi

# Check if PostgreSQL is accepting connections
CONN_ERROR=$(mktemp)
if $PSQL_CMD -c "SELECT 1" >/dev/null 2>"$CONN_ERROR"; then
  STATUS="ok"
  MESSAGE="PostgreSQL is accepting connections"
  rm -f "$CONN_ERROR"
else
  STATUS="error"
  ERROR_MSG=$(cat "$CONN_ERROR" 2>/dev/null | head -1)
  rm -f "$CONN_ERROR"
  if [ -n "$ERROR_MSG" ]; then
    MESSAGE="PostgreSQL is not accepting connections: $ERROR_MSG"
  else
    MESSAGE="PostgreSQL is not accepting connections"
  fi
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  '{status: $status, message: $message, data: {}}'
