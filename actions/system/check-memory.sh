#!/usr/bin/env bash
# Action: system/check-memory
# Description: Check memory usage percentage
# Parameters:
#   - threshold (optional, default: 90): Warning threshold percentage

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
THRESHOLD=$(echo "$CONFIG" | jq -r '.threshold // 90')

# Check memory
MEM_TOTAL=$(awk '/MemTotal/ {print $2}' /proc/meminfo)
MEM_AVAIL=$(awk '/MemAvailable/ {print $2}' /proc/meminfo)
MEM_USED_PCT=$((100 - (MEM_AVAIL * 100 / MEM_TOTAL)))

# Determine status
if [ "$MEM_USED_PCT" -lt "$THRESHOLD" ]; then
  STATUS="ok"
  MESSAGE="Memory usage at ${MEM_USED_PCT}% (threshold: ${THRESHOLD}%)"
else
  STATUS="warning"
  MESSAGE="High memory usage: ${MEM_USED_PCT}% (threshold: ${THRESHOLD}%)"
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  --argjson memory_used_pct "$MEM_USED_PCT" \
  --argjson threshold "$THRESHOLD" \
  '{status: $status, message: $message, data: {memory_used_pct: $memory_used_pct, threshold: $threshold}}'
