#!/usr/bin/env bash
# Emacs invocation utilities

source "$(dirname "${BASH_SOURCE[0]}")/common.bash"

# Run Emacs in batch mode
emacs_batch() {
  local emacs
  emacs=$(get_emacs)

  "$emacs" --batch \
    --eval "(setq jotain-batch-mode t)" \
    "$@"
}

# Run Emacs with Jotain loaded in batch mode
emacs_batch_jotain() {
  local emacs jotain_root
  emacs=$(get_emacs)
  jotain_root=$(get_jotain_root)

  "$emacs" --batch \
    --eval "(setq jotain-batch-mode t)" \
    --eval "(add-to-list 'load-path \"$jotain_root/elisp\")" \
    --eval "(add-to-list 'load-path \"$jotain_root/elisp/core\")" \
    --eval "(add-to-list 'load-path \"$jotain_root/elisp/ui\")" \
    --eval "(add-to-list 'load-path \"$jotain_root/elisp/completion\")" \
    --eval "(add-to-list 'load-path \"$jotain_root/elisp/editor\")" \
    --eval "(add-to-list 'load-path \"$jotain_root/elisp/programming\")" \
    --eval "(require 'jotain nil t)" \
    "$@"
}

# Evaluate elisp expression
emacs_eval() {
  emacs_batch_jotain --eval "$1" 2>&1 | tail -n 1
}

# Check if Emacs can load Jotain
emacs_can_load_jotain() {
  emacs_batch_jotain --eval '(message "OK")' 2>&1 | grep -q "OK"
}

# Get Jotain version
get_jotain_version() {
  emacs_batch_jotain --eval "(progn (require 'jotain) (princ jotain-version))" 2>/dev/null || echo "unknown"
}

# Run Emacs interactively with Jotain
emacs_run() {
  local emacs jotain_root
  emacs=$(get_emacs)
  jotain_root=$(get_jotain_root)

  if is_dev_mode; then
    info "Running in development mode"
    export JOTAIN_DEV_MODE=1
    export JOTAIN_ROOT="$jotain_root"
  fi

  "$emacs" "$@"
}
