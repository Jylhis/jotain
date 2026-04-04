#!/usr/bin/env bash
# elisp-lint.sh — Fast regex-based lint checks for .el files
# Used as a Claude Code post-edit hook on .el files in jotain.
set -euo pipefail

FILE="$1"
BASENAME=$(basename "$FILE" .el)
WARNINGS=0
ERRORS=0

# 1. lexical-binding on line 1
if ! head -1 "$FILE" | grep -q 'lexical-binding.*t'; then
  echo "ERROR [$FILE:1]: Missing -*- lexical-binding:t; -*-"
  ERRORS=$((ERRORS + 1))
fi

# 2. provide statement matching filename
if ! grep -q "(provide '$BASENAME)" "$FILE"; then
  echo "ERROR [$FILE]: Missing (provide '$BASENAME)"
  ERRORS=$((ERRORS + 1))
fi

# 3. Footer comment
if ! tail -1 "$FILE" | grep -q ";;; .* ends here"; then
  echo "WARN [$FILE]: Missing ;;; $BASENAME.el ends here"
  WARNINGS=$((WARNINGS + 1))
fi

# 4. eval-after-load without with-
if grep -n '(eval-after-load ' "$FILE" | grep -v 'with-eval-after-load' | head -3; then
  echo "WARN: Use with-eval-after-load instead of eval-after-load"
  WARNINGS=$((WARNINGS + 1))
fi

# 5. define-key (legacy)
if grep -qn '(define-key ' "$FILE"; then
  grep -n '(define-key ' "$FILE" | head -3
  echo "WARN: Use keymap-set instead of define-key"
  WARNINGS=$((WARNINGS + 1))
fi

# 6. global-set-key / local-set-key (legacy)
if grep -qn '(global-set-key \|(local-set-key ' "$FILE"; then
  grep -n '(global-set-key \|(local-set-key ' "$FILE" | head -3
  echo "WARN: Use keymap-global-set/keymap-local-set instead"
  WARNINGS=$((WARNINGS + 1))
fi

# 7. require 'cl (obsolete)
if grep -qn "(require 'cl)" "$FILE"; then
  grep -n "(require 'cl)" "$FILE"
  echo "ERROR: Use cl-lib instead of cl"
  ERRORS=$((ERRORS + 1))
fi

# 8. Lambda in add-hook
if grep -qn "(add-hook '.*-hook.*(lambda" "$FILE"; then
  grep -n "(add-hook '.*-hook.*(lambda" "$FILE" | head -3
  echo "WARN: Use named functions in add-hook, not lambdas"
  WARNINGS=$((WARNINGS + 1))
fi

# Summary
if [ $ERRORS -gt 0 ] || [ $WARNINGS -gt 0 ]; then
  echo "---"
  echo "elisp-lint: $ERRORS error(s), $WARNINGS warning(s)"
fi

# Only fail on errors, not warnings
exit $ERRORS
