#!/usr/bin/env bash
# Action: system/check-disk
# Description: Check disk usage percentage for a given path
# Parameters:
#   - path (required): Mount point to check
#   - threshold (optional, default: 90): Warning threshold percentage

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
PATH_TO_CHECK=$(echo "$CONFIG" | jq -r '.path')
THRESHOLD=$(echo "$CONFIG" | jq -r '.threshold // 90')

# Validate inputs
if [ -z "$PATH_TO_CHECK" ] || [ "$PATH_TO_CHECK" = "null" ]; then
  jq -n \
    --arg status "error" \
    --arg message "Missing required parameter: path" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Check disk usage
USAGE=$(df "$PATH_TO_CHECK" | awk 'NR==2 {print int($5)}' 2>/dev/null || echo "error")

if [ "$USAGE" = "error" ]; then
  jq -n \
    --arg status "error" \
    --arg message "Failed to check disk usage for: $PATH_TO_CHECK" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Determine status
if [ "$USAGE" -lt "$THRESHOLD" ]; then
  STATUS="ok"
  MESSAGE="Disk usage at ${USAGE}% (threshold: ${THRESHOLD}%)"
else
  STATUS="error"
  MESSAGE="Disk usage at ${USAGE}% exceeds threshold of ${THRESHOLD}%"
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  --argjson usage "$USAGE" \
  --argjson threshold "$THRESHOLD" \
  --arg path "$PATH_TO_CHECK" \
  '{status: $status, message: $message, data: {usage: $usage, threshold: $threshold, path: $path}}'
