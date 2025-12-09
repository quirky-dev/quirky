#!/usr/bin/env bash
# Action: network/ssh-dir-size
# Description: Check size of a remote directory and optionally compare to local directory
# Parameters:
#   - host (required): Remote host
#   - port (optional, default: 22): SSH port
#   - user (required): SSH user
#   - ssh_key (required): Path to SSH private key
#   - known_hosts_file (optional): Path to known_hosts file for host verification
#   - remote_path (required): Remote directory path to check
#   - local_path (optional): Local directory to compare against
#   - max_diff_percent (optional, default: 10): Maximum acceptable size difference percentage
#   - min_size_mb (optional, default: 1): Minimum acceptable size in MB

set -euo pipefail

# Read config from stdin
CONFIG=$(cat)
HOST=$(echo "$CONFIG" | jq -r '.host')
PORT=$(echo "$CONFIG" | jq -r '.port // 22')
USER=$(echo "$CONFIG" | jq -r '.user')
SSH_KEY=$(echo "$CONFIG" | jq -r '.ssh_key')
KNOWN_HOSTS_FILE=$(echo "$CONFIG" | jq -r '.known_hosts_file // empty')
REMOTE_PATH=$(echo "$CONFIG" | jq -r '.remote_path')
LOCAL_PATH=$(echo "$CONFIG" | jq -r '.local_path // empty')
MAX_DIFF_PERCENT=$(echo "$CONFIG" | jq -r '.max_diff_percent // 10')
MIN_SIZE_MB=$(echo "$CONFIG" | jq -r '.min_size_mb // 1')

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

# Build SSH options
# -T: Disable pseudo-terminal allocation (non-interactive)
SSH_OPTS="-T -i $SSH_KEY -p $PORT"
if [ -n "$KNOWN_HOSTS_FILE" ]; then
  # Use provided known_hosts file with strict checking
  SSH_OPTS="$SSH_OPTS -o UserKnownHostsFile=$KNOWN_HOSTS_FILE -o StrictHostKeyChecking=yes"
else
  # No known_hosts file provided - disable strict checking
  SSH_OPTS="$SSH_OPTS -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
fi

# Get the size of the remote directory
# Use du with --apparent-size to get actual file sizes (not disk usage)
# This avoids issues with compression/sparse files on different filesystems
DU_OUTPUT=$(ssh $SSH_OPTS "$USER@$HOST" \
  "du -s --apparent-size '$REMOTE_PATH' 2>/dev/null" 2>&1 || echo "")

# Check for SSH errors
if [ -z "$DU_OUTPUT" ] || echo "$DU_OUTPUT" | grep -q "Permission denied\|Connection refused\|Could not resolve\|No such file"; then
  jq -n \
    --arg status "error" \
    --arg message "SSH error or directory not found: $DU_OUTPUT" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Parse du output (format: "SIZE_IN_KB PATH")
# Extract the size (first field)
SIZE_KB=$(echo "$DU_OUTPUT" | awk '{print $1}')

# Validate SIZE_KB is a number
if ! [[ "$SIZE_KB" =~ ^[0-9]+$ ]]; then
  jq -n \
    --arg status "error" \
    --arg message "Invalid size received: $DU_OUTPUT" \
    '{status: $status, message: $message, data: {}}'
  exit 0
fi

# Convert to MB
REMOTE_SIZE_MB=$((SIZE_KB / 1024))

# Format for display
if [ "$REMOTE_SIZE_MB" -gt 1024 ]; then
  REMOTE_SIZE_GB=$((REMOTE_SIZE_MB / 1024))
  REMOTE_DISPLAY="${REMOTE_SIZE_GB}GB"
else
  REMOTE_DISPLAY="${REMOTE_SIZE_MB}MB"
fi

# Check minimum size threshold
if [ "$REMOTE_SIZE_MB" -lt "$MIN_SIZE_MB" ]; then
  STATUS="error"
  MESSAGE="Remote directory size is ${REMOTE_DISPLAY} - below minimum threshold of ${MIN_SIZE_MB}MB"

  jq -n \
    --arg status "$STATUS" \
    --arg message "$MESSAGE" \
    --argjson remote_size_mb "$REMOTE_SIZE_MB" \
    --arg remote_display "$REMOTE_DISPLAY" \
    '{status: $status, message: $message, data: {remote_size_mb: $remote_size_mb, remote_display: $remote_display}}'
  exit 0
fi

# If local_path is provided, compare sizes
if [ -n "$LOCAL_PATH" ]; then
  if [ ! -d "$LOCAL_PATH" ]; then
    jq -n \
      --arg status "error" \
      --arg message "Local directory not found: $LOCAL_PATH" \
      '{status: $status, message: $message, data: {}}'
    exit 0
  fi

  # Get local directory size (using --apparent-size to match remote)
  LOCAL_SIZE_KB=$(du -s --apparent-size "$LOCAL_PATH" 2>/dev/null | awk '{print $1}')
  LOCAL_SIZE_MB=$((LOCAL_SIZE_KB / 1024))

  if [ "$LOCAL_SIZE_MB" -gt 1024 ]; then
    LOCAL_SIZE_GB=$((LOCAL_SIZE_MB / 1024))
    LOCAL_DISPLAY="${LOCAL_SIZE_GB}GB"
  else
    LOCAL_DISPLAY="${LOCAL_SIZE_MB}MB"
  fi

  # Calculate difference percentage
  if [ "$LOCAL_SIZE_MB" -gt 0 ]; then
    DIFF_MB=$((LOCAL_SIZE_MB - REMOTE_SIZE_MB))
    # Use absolute value
    DIFF_MB=${DIFF_MB#-}
    DIFF_PERCENT=$((DIFF_MB * 100 / LOCAL_SIZE_MB))
  else
    DIFF_PERCENT=100
  fi

  # Check if difference is acceptable
  if [ "$DIFF_PERCENT" -gt "$MAX_DIFF_PERCENT" ]; then
    STATUS="error"
    MESSAGE="Size mismatch: local=${LOCAL_DISPLAY}, remote=${REMOTE_DISPLAY} (${DIFF_PERCENT}% difference, max ${MAX_DIFF_PERCENT}%)"
  else
    STATUS="ok"
    MESSAGE="Sizes match: local=${LOCAL_DISPLAY}, remote=${REMOTE_DISPLAY} (${DIFF_PERCENT}% difference)"
  fi

  jq -n \
    --arg status "$STATUS" \
    --arg message "$MESSAGE" \
    --argjson local_size_mb "$LOCAL_SIZE_MB" \
    --arg local_display "$LOCAL_DISPLAY" \
    --argjson remote_size_mb "$REMOTE_SIZE_MB" \
    --arg remote_display "$REMOTE_DISPLAY" \
    --argjson diff_percent "$DIFF_PERCENT" \
    '{status: $status, message: $message, data: {local_size_mb: $local_size_mb, local_display: $local_display, remote_size_mb: $remote_size_mb, remote_display: $remote_display, diff_percent: $diff_percent}}'
else
  # No local path - just check remote size
  STATUS="ok"
  MESSAGE="Remote directory size is ${REMOTE_DISPLAY}"

  jq -n \
    --arg status "$STATUS" \
    --arg message "$MESSAGE" \
    --argjson remote_size_mb "$REMOTE_SIZE_MB" \
    --arg remote_display "$REMOTE_DISPLAY" \
    '{status: $status, message: $message, data: {remote_size_mb: $remote_size_mb, remote_display: $remote_display}}'
fi
