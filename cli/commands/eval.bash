#!/usr/bin/env bash
# Evaluate Emacs Lisp expression

source "$(dirname "${BASH_SOURCE[0]}")/../lib/common.bash"
source "$(dirname "${BASH_SOURCE[0]}")/../lib/emacs.bash"

eval_command() {
  if [ $# -eq 0 ]; then
    die "Usage: jot eval EXPRESSION"
  fi

  local expression="$*"
  emacs_batch_jotain --eval "(princ $expression)"
}
