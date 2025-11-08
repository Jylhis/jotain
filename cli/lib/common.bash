#!/usr/bin/env bash
# Common utilities for Jotain CLI

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
info() {
  echo -e "${BLUE}==>${NC} $*"
}

success() {
  echo -e "${GREEN}==>${NC} $*"
}

warn() {
  echo -e "${YELLOW}Warning:${NC} $*" >&2
}

error() {
  echo -e "${RED}Error:${NC} $*" >&2
}

die() {
  error "$@"
  exit 1
}

# Check if command exists
has_command() {
  command -v "$1" >/dev/null 2>&1
}

# Detect if in development mode
is_dev_mode() {
  [ -n "$JOTAIN_DEV_MODE" ] || [ -f "$JOTAIN_ROOT/.git/config" ]
}

# Get Jotain root directory
get_jotain_root() {
  if [ -n "$JOTAIN_ROOT" ]; then
    echo "$JOTAIN_ROOT"
  elif [ -n "$JOTAIN_HOME" ]; then
    echo "$JOTAIN_HOME"
  else
    echo "$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
  fi
}

# Get Emacs executable
get_emacs() {
  if has_command emacs; then
    echo "emacs"
  else
    die "Emacs not found in PATH"
  fi
}
