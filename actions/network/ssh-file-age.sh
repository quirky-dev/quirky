#!/usr/bin/env bash
# Action: network/ssh-file-age
# Description: Check age of files on remote SSH server (e.g., backup files)
# Parameters:
#   - host (required): Remote host
#   - port (optional, default: 22): SSH port
#   - user (required): SSH user
#   - ssh_key (required): Path to SSH private key
#   - known_hosts_file (optional): Path to known_hosts file for host verification
#   - remote_path (required): Remote path to check
#   - databases (required): Array of database names (subdirectories)
#   - max_age_hours (optional, default: 24): Maximum acceptable file age in hours

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
HOST=$(echo "$CONFIG" | jq -r '.host')
PORT=$(echo "$CONFIG" | jq -r '.port // 22')
USER=$(echo "$CONFIG" | jq -r '.user')
SSH_KEY=$(echo "$CONFIG" | jq -r '.ssh_key')
KNOWN_HOSTS_FILE=$(echo "$CONFIG" | jq -r '.known_hosts_file // empty')
REMOTE_PATH=$(echo "$CONFIG" | jq -r '.remote_path')
DATABASES=$(echo "$CONFIG" | jq -r '.databases[]' 2>/dev/null || echo "")
MAX_AGE_HOURS=$(echo "$CONFIG" | jq -r '.max_age_hours // 24')

# Validate inputs
if [ -z "$HOST" ] || [ "$HOST" = "null" ]; then
  jq -n --arg status "error" --arg message "Missing required parameter: host" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

if [ -z "$USER" ] || [ "$USER" = "null" ]; then
  jq -n --arg status "error" --arg message "Missing required parameter: user" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

if [ -z "$SSH_KEY" ] || [ "$SSH_KEY" = "null" ]; then
  jq -n --arg status "error" --arg message "Missing required parameter: ssh_key" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

if [ -z "$REMOTE_PATH" ] || [ "$REMOTE_PATH" = "null" ]; then
  jq -n --arg status "error" --arg message "Missing required parameter: remote_path" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

if [ -z "$DATABASES" ]; then
  jq -n --arg status "error" --arg message "Missing required parameter: databases" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Build SSH options
SSH_OPTS="-i $SSH_KEY -p $PORT"
if [ -n "$KNOWN_HOSTS_FILE" ]; then
  # Use provided known_hosts file with strict checking
  SSH_OPTS="$SSH_OPTS -o UserKnownHostsFile=$KNOWN_HOSTS_FILE -o StrictHostKeyChecking=yes"
else
  # No known_hosts file provided - disable strict checking
  SSH_OPTS="$SSH_OPTS -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
fi

# Check for dumps of all databases
ALL_OK=true
MESSAGES=()
TODAY=$(date +%Y%m%d)
YESTERDAY=$(date -d "yesterday" +%Y%m%d)

while IFS= read -r DB; do
  [ -z "$DB" ] && continue

  # List remote files for this database, sorted by filename (in subdirectory)
  LATEST=$(ssh $SSH_OPTS "$USER@$HOST" \
    "ls -1 $REMOTE_PATH/$DB/*.sql.gz 2>/dev/null | sort -r | head -1" 2>&1 || echo "")

  if [ -z "$LATEST" ]; then
    ALL_OK=false
    MESSAGES+=("No backup found for $DB")
  elif echo "$LATEST" | grep -q "Permission denied\|Connection refused\|Could not resolve"; then
    ALL_OK=false
    MESSAGES+=("SSH error for $DB: $LATEST")
  else
    # Extract date from filename (format: db_YYYYMMDD_HHMMSS.sql.gz)
    BACKUP_DATE=$(basename "$LATEST" | grep -oP '\d{8}' | head -1)

    if [ "$BACKUP_DATE" = "$TODAY" ] || [ "$BACKUP_DATE" = "$YESTERDAY" ]; then
      FORMATTED_DATE="${BACKUP_DATE:0:4}-${BACKUP_DATE:4:2}-${BACKUP_DATE:6:2}"
      MESSAGES+=("$DB: backup from $FORMATTED_DATE")
    else
      ALL_OK=false
      FORMATTED_DATE="${BACKUP_DATE:0:4}-${BACKUP_DATE:4:2}-${BACKUP_DATE:6:2}"
      MESSAGES+=("$DB: latest backup is from $FORMATTED_DATE (too old)")
    fi
  fi
done < <(echo "$CONFIG" | jq -r '.databases[]')

if [ "$ALL_OK" = true ]; then
  STATUS="ok"
else
  STATUS="error"
fi

# Join messages with semicolons
MESSAGE=$(IFS="; "; echo "${MESSAGES[*]}")

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  '{status: $status, message: $message, data: {}}'
