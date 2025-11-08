#!/usr/bin/env bash
# Check Jotain installation health

source "$(dirname "${BASH_SOURCE[0]}")/../lib/common.bash"
source "$(dirname "${BASH_SOURCE[0]}")/../lib/emacs.bash"

doctor_command() {
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "  Jotain Health Check"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo

  local issues=0

  # Check Emacs
  if has_command emacs; then
    local emacs_version
    emacs_version=$(emacs --version | head -n1)
    success "Emacs found: $emacs_version"
  else
    error "Emacs not found in PATH"
    ((issues++))
  fi

  # Check Emacs version
  local version
  version=$(emacs --batch --eval "(princ emacs-version)" 2>/dev/null)
  if [[ $version =~ ^([0-9]+) ]]; then
    local major="${BASH_REMATCH[1]}"
    if [ "$major" -ge 30 ]; then
      success "Emacs version OK: $version (>= 30 required)"
    else
      warn "Emacs version $version detected (30+ recommended)"
    fi
  fi

  # Check Jotain installation
  local jotain_root
  jotain_root=$(get_jotain_root)
  if [ -d "$jotain_root/elisp" ]; then
    success "Jotain installation found: $jotain_root"
  else
    error "Jotain elisp directory not found at: $jotain_root/elisp"
    ((issues++))
  fi

  # Check if Jotain can load
  if emacs_can_load_jotain; then
    local version
    version=$(get_jotain_version)
    success "Jotain loads successfully: version $version"
  else
    error "Jotain failed to load"
    ((issues++))
  fi

  # Check development mode
  if is_dev_mode; then
    info "Development mode: enabled"
  else
    info "Development mode: disabled"
  fi

  # Check for LSP servers
  echo
  echo "Optional tools:"
  if has_command nil; then
    success "nil (Nix LSP): found"
  else
    warn "nil (Nix LSP): not found (recommended for Nix editing)"
  fi

  if has_command bash-language-server; then
    success "bash-language-server: found"
  else
    warn "bash-language-server: not found (recommended for shell editing)"
  fi

  # Summary
  echo
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  if [ $issues -eq 0 ]; then
    success "Health check passed! Jotain is ready to use."
    return 0
  else
    error "Health check found $issues issue(s)"
    return 1
  fi
}
