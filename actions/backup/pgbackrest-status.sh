#!/usr/bin/env bash
# Action: backup/pgbackrest-status
# Description: Check pgBackRest backup status
# Parameters:
#   - stanza (required): pgBackRest stanza name
#   - age_hours (optional, default: 48): Maximum acceptable backup age in hours
#   - user (optional, default: postgres): User to run as

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
STANZA=$(echo "$CONFIG" | jq -r '.stanza')
AGE_HOURS=$(echo "$CONFIG" | jq -r '.age_hours // 48')
USER=$(echo "$CONFIG" | jq -r '.user // "postgres"')

# Validate inputs
if [ -z "$STANZA" ] || [ "$STANZA" = "null" ]; then
  jq -n \
    --arg status "error" \
    --arg message "Missing required parameter: stanza" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Get pgbackrest info
INFO=$(sudo -u "$USER" pgbackrest --stanza="$STANZA" info 2>&1 || echo "ERROR")

if echo "$INFO" | grep -q "ERROR"; then
  STATUS="error"
  MESSAGE="pgBackRest error: $INFO"
else
  # Check for recent backup of any type
  # Format: "full backup: 20251130-135447F" or "diff backup: 20251130-135447F_20251204-020149D"
  LATEST_BACKUP=$(echo "$INFO" | grep -E "(full|diff|incr) backup:" | tail -1 | awk '{print $3}')

  if [ -z "$LATEST_BACKUP" ]; then
    STATUS="error"
    MESSAGE="No backups found"
  else
    # For diff/incr backups, extract the part after underscore
    if [[ "$LATEST_BACKUP" == *_* ]]; then
      BACKUP_TIMESTAMP=$(echo "$LATEST_BACKUP" | cut -d'_' -f2)
    else
      BACKUP_TIMESTAMP="$LATEST_BACKUP"
    fi

    # Strip trailing letter (F/D/I)
    BACKUP_TIMESTAMP="${BACKUP_TIMESTAMP%[FDI]}"
    BACKUP_DATE=$(echo "$BACKUP_TIMESTAMP" | cut -d'-' -f1)
    BACKUP_TIME=$(echo "$BACKUP_TIMESTAMP" | cut -d'-' -f2)

    # Convert to epoch time
    BACKUP_EPOCH=$(date -d "${BACKUP_DATE:0:4}-${BACKUP_DATE:4:2}-${BACKUP_DATE:6:2} ${BACKUP_TIME:0:2}:${BACKUP_TIME:2:2}:${BACKUP_TIME:4:2}" +%s 2>/dev/null || echo "0")
    NOW=$(date +%s)
    BACKUP_AGE_HOURS=$(( (NOW - BACKUP_EPOCH) / 3600 ))

    if [ "$BACKUP_AGE_HOURS" -lt "$AGE_HOURS" ]; then
      STATUS="ok"
      MESSAGE="Latest backup: $LATEST_BACKUP ($BACKUP_AGE_HOURS hours ago)"
    else
      STATUS="error"
      MESSAGE="Latest backup is $BACKUP_AGE_HOURS hours old (threshold: ${AGE_HOURS}h)"
    fi
  fi
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  --argjson age_hours "${BACKUP_AGE_HOURS:-999999}" \
  '{status: $status, message: $message, data: {age_hours: $age_hours}}'
