#!/usr/bin/env bash
# nix-eval-check.sh — Verify Nix expression evaluates without errors
# Intended as a pre-commit check for jotain.
set -euo pipefail

echo "Checking Nix evaluation..."

# Check that default.nix evaluates (catches attribute errors, infinite recursion)
if ! nix-instantiate default.nix 2>/tmp/nix-eval-errors.txt; then
  echo "ERROR: Nix evaluation failed:"
  cat /tmp/nix-eval-errors.txt
  exit 1
fi

# Check that emacs.nix evaluates independently
if ! nix-instantiate emacs.nix 2>/tmp/nix-eval-errors.txt; then
  echo "ERROR: emacs.nix evaluation failed:"
  cat /tmp/nix-eval-errors.txt
  exit 1
fi

echo "Nix evaluation: OK"
