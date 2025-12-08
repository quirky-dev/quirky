#!/usr/bin/env bash
# Action: backup/ssh-dir-age
# Description: Check modification time of a remote directory (e.g., rsync backup target)
# Parameters:
#   - host (required): Remote host
#   - port (optional, default: 22): SSH port
#   - user (required): SSH user
#   - ssh_key (required): Path to SSH private key
#   - remote_path (required): Remote directory path to check
#   - max_age_hours (optional, default: 24): Maximum acceptable age in hours

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
HOST=$(echo "$CONFIG" | jq -r '.host')
PORT=$(echo "$CONFIG" | jq -r '.port // 22')
USER=$(echo "$CONFIG" | jq -r '.user')
SSH_KEY=$(echo "$CONFIG" | jq -r '.ssh_key')
REMOTE_PATH=$(echo "$CONFIG" | jq -r '.remote_path')
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

# Get the modification time of the remote directory
# Use stat to get mtime as Unix timestamp
MTIME=$(sudo -u quirky ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no \
  -i "$SSH_KEY" -p "$PORT" "$USER@$HOST" \
  "stat -c %Y '$REMOTE_PATH' 2>/dev/null" 2>&1 || echo "")

# Check for SSH errors
if [ -z "$MTIME" ] || echo "$MTIME" | grep -q "Permission denied\|Connection refused\|Could not resolve\|No such file"; then
  jq -n \
    --arg status "error" \
    --arg message "SSH error or directory not found: $MTIME" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Validate MTIME is a number
if ! [[ "$MTIME" =~ ^[0-9]+$ ]]; then
  jq -n \
    --arg status "error" \
    --arg message "Invalid timestamp received: $MTIME" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Calculate age in hours
NOW=$(date +%s)
AGE_SECONDS=$((NOW - MTIME))
AGE_HOURS=$((AGE_SECONDS / 3600))

# Format the last modified time for display
LAST_MODIFIED=$(date -d "@$MTIME" "+%Y-%m-%d %H:%M:%S")

# Check if age exceeds threshold
if [ "$AGE_HOURS" -gt "$MAX_AGE_HOURS" ]; then
  STATUS="error"
  MESSAGE="Directory last modified $AGE_HOURS hours ago ($LAST_MODIFIED) - exceeds threshold of $MAX_AGE_HOURS hours"
else
  STATUS="ok"
  MESSAGE="Directory last modified $AGE_HOURS hours ago ($LAST_MODIFIED)"
fi

jq -n \
  --arg status "$STATUS" \
  --arg message "$MESSAGE" \
  --argjson age_hours "$AGE_HOURS" \
  --arg last_modified "$LAST_MODIFIED" \
  '{status: $status, message: $message, data: {age_hours: $age_hours, last_modified: $last_modified}}'
