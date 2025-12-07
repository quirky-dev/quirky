#!/usr/bin/env bash
# Action: database/postgres-query-age
# Description: Check age of data returned by a PostgreSQL query (e.g., last updated timestamp)
# Parameters:
#   - database (required): Database to query
#   - query (required): SQL query that returns a timestamp
#   - max_age_hours (optional, default: 48): Maximum acceptable age in hours
#   - user (optional, default: postgres): User to connect as

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
DATABASE=$(echo "$CONFIG" | jq -r '.database')
QUERY=$(echo "$CONFIG" | jq -r '.query')
MAX_AGE_HOURS=$(echo "$CONFIG" | jq -r '.max_age_hours // 48')
USER=$(echo "$CONFIG" | jq -r '.user // "postgres"')

# Validate inputs
if [ -z "$DATABASE" ] || [ "$DATABASE" = "null" ]; then
  jq -n \
    --arg status "error" \
    --arg message "Missing required parameter: database" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

if [ -z "$QUERY" ] || [ "$QUERY" = "null" ]; then
  jq -n \
    --arg status "error" \
    --arg message "Missing required parameter: query" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Query for the timestamp
QUERY_ERROR=$(mktemp)
RESULT=$(sudo -u "$USER" psql -d "$DATABASE" -t -c "$QUERY" 2>"$QUERY_ERROR" || echo "")

if [ -z "$RESULT" ]; then
  STATUS="error"
  ERROR_MSG=$(cat "$QUERY_ERROR" 2>/dev/null | head -1)
  rm -f "$QUERY_ERROR"
  if [ -n "$ERROR_MSG" ]; then
    MESSAGE="Could not query database: $ERROR_MSG"
  else
    MESSAGE="Could not query database or no results returned"
  fi
else
  rm -f "$QUERY_ERROR"
  # Parse the timestamp
  RESULT_SECONDS=$(date -d "$RESULT" +%s 2>/dev/null || echo "0")
  NOW_SECONDS=$(date +%s)
  AGE_HOURS=$(( (NOW_SECONDS - RESULT_SECONDS) / 3600 ))

  if [ "$AGE_HOURS" -lt "$MAX_AGE_HOURS" ]; then
    STATUS="ok"
    MESSAGE="Data age: $AGE_HOURS hours (threshold: ${MAX_AGE_HOURS}h)"
  else
    STATUS="error"
    MESSAGE="Data is $AGE_HOURS hours old (threshold: ${MAX_AGE_HOURS}h)"
  fi
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  --argjson age_hours "${AGE_HOURS:-999999}" \
  '{status: $status, message: $message, data: {age_hours: $age_hours}}'
