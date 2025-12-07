#!/usr/bin/env bash
# Action: system/check-load
# Description: Check CPU load average
# Parameters:
#   - threshold_multiplier (optional, default: 2): Warn if load > cores * multiplier

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
THRESHOLD_MULTIPLIER=$(echo "$CONFIG" | jq -r '.threshold_multiplier // 2')

# Check load average and core count
LOAD=$(awk '{print $1}' /proc/loadavg)
CORES=$(nproc)

# Calculate threshold (cores * multiplier)
THRESHOLD=$(echo "$CORES * $THRESHOLD_MULTIPLIER" | bc)

# Compare load to threshold
LOAD_EXCEEDS=$(echo "$LOAD > $THRESHOLD" | bc)

if [ "$LOAD_EXCEEDS" = "1" ]; then
  STATUS="warning"
  MESSAGE="High load: ${LOAD} (${CORES} cores, threshold: ${THRESHOLD})"
else
  STATUS="ok"
  MESSAGE="Load: ${LOAD} (${CORES} cores, threshold: ${THRESHOLD})"
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  --arg load "$LOAD" \
  --argjson cores "$CORES" \
  --arg threshold "$THRESHOLD" \
  '{status: $status, message: $message, data: {load: $load, cores: $cores, threshold: $threshold}}'
