#!/usr/bin/env bash
# Action: database/postgres-replication
# Description: Check PostgreSQL streaming replication status and lag
# Parameters:
#   - lag_threshold_mb (optional, default: 100): Warning threshold for replication lag in MB
#   - user (optional, default: postgres): User to connect as

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
LAG_THRESHOLD_MB=$(echo "$CONFIG" | jq -r '.lag_threshold_mb // 100')
USER=$(echo "$CONFIG" | jq -r '.user // "postgres"')

# Check if streaming replication is active
REPLICATION_ERROR=$(mktemp)
REPLICATION_STATUS=$(sudo -u "$USER" psql -t -c \
  "SELECT status FROM pg_stat_wal_receiver" 2>"$REPLICATION_ERROR" || echo "")

if [ -z "$REPLICATION_STATUS" ]; then
  STATUS="error"
  ERROR_MSG=$(cat "$REPLICATION_ERROR" 2>/dev/null | head -1)
  rm -f "$REPLICATION_ERROR"
  if [ -n "$ERROR_MSG" ]; then
    MESSAGE="Replication status unavailable: $ERROR_MSG"
  else
    MESSAGE="Replication status unavailable"
  fi
elif echo "$REPLICATION_STATUS" | grep -q "streaming"; then
  rm -f "$REPLICATION_ERROR"
  # Check replication lag
  LAG_BYTES=$(sudo -u "$USER" psql -t -c \
    "SELECT COALESCE(pg_wal_lsn_diff(pg_last_wal_receive_lsn(), pg_last_wal_replay_lsn()), 0)" 2>/dev/null || echo "0")

  LAG_MB=$((LAG_BYTES / 1048576))

  if [ "$LAG_MB" -lt "$LAG_THRESHOLD_MB" ]; then
    STATUS="ok"
    MESSAGE="Replication streaming, lag: ${LAG_MB}MB"
  else
    STATUS="warning"
    MESSAGE="Replication streaming but lagging: ${LAG_MB}MB (threshold: ${LAG_THRESHOLD_MB}MB)"
  fi
else
  rm -f "$REPLICATION_ERROR"
  STATUS="error"
  MESSAGE="Replication not streaming: $REPLICATION_STATUS"
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  --argjson lag_mb "${LAG_MB:-0}" \
  '{status: $status, message: $message, data: {lag_mb: $lag_mb}}'
