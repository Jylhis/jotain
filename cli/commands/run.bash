#!/usr/bin/env bash
# Run Emacs with Jotain configuration

source "$(dirname "${BASH_SOURCE[0]}")/../lib/common.bash"
source "$(dirname "${BASH_SOURCE[0]}")/../lib/emacs.bash"

run_command() {
  info "Launching Emacs with Jotain..."
  emacs_run "$@"
}
