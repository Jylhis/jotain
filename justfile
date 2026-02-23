config_dir := justfile_directory()
# Project directory for cleaning/compilation (not user's home!)
emacs_config_dir := config_dir
# Isolated dev environment directory
dev_home := config_dir + '/.dev-home'
system := `nix eval --impure --raw --expr 'builtins.currentSystem'`

# Default command - show available commands
default:
    @just --list --justfile {{justfile()}}

# Development and Testing
# =====================

# Check Emacs configuration syntax (runs Nix build dry-run)
[group('check')]
check:
    nix build --dry-run .#default

# Run ultra-fast smoke tests (< 1 second)
[group('check')]
test-smoke:
    @echo "Running smoke tests (<  1 second)..."
    nix build .#checks.{{system}}.smoke-test --no-link --print-build-logs

# Run fast unit tests (< 5 seconds, excludes slow filesystem tests)
[group('check')]
test-fast:
    @echo "Running fast unit tests (< 5 seconds)..."
    nix build .#checks.{{system}}.fast-tests --no-link --print-build-logs

# Run ERT unit tests via Nix (full suite including slow tests)
[group('check')]
test:
    @echo "Running full ERT test suite..."
    nix build .#checks.{{system}}.tests --no-link --print-build-logs

# Run all fast checks (formatting + smoke + fast tests + NMT, excludes VM)
[group('check')]
test-all:
    @echo "Running all fast checks (excludes VM runtime tests)..."
    nix flake check --print-build-logs

# Run ALL tests including slow VM runtime validation
[group('check')]
test-all-plus-runtime:
    @echo "Running ALL tests including VM runtime validation..."
    CI=1 nix flake check --print-build-logs

# Run checks in parallel (faster on multi-core systems)
[group('check')]
check-parallel:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Running fast checks in parallel..."
    nix build \
      .#checks.{{system}}.formatting \
      .#checks.{{system}}.smoke-test \
      .#checks.{{system}}.fast-tests \
      .#checks.{{system}}.tests \
      .#checks.{{system}}.test-module-enabled \
      .#checks.{{system}}.test-module-disabled \
      --keep-going \
      --print-build-logs \
      -j auto
    echo "All fast checks passed!"

# Instant feedback checks (< 10 seconds)
[group('check')]
check-instant:
    @echo "Running instant checks (< 10 seconds)..."
    nix build .#checks.{{system}}.formatting --print-build-logs
    nix build .#checks.{{system}}.smoke-test --no-link --print-build-logs

# Fast validation (< 1 minute)
[group('check')]
check-fast:
    @echo "Running fast validation (< 1 minute)..."
    @just check-instant
    nix build .#checks.{{system}}.fast-tests --no-link --print-build-logs

# Full local testing (< 5 minutes, excludes VM)
[group('check')]
check-full:
    @echo "Running full local testing..."
    nix flake check --print-build-logs

# Complete validation (includes VM runtime tests)
[group('check')]
check-all:
    @echo "Running complete validation (includes VM)..."
    @just test-all-plus-runtime

# Run NMT home-manager module tests (all 5 cases)
[group('check')]
test-nmt:
    @echo "Running NMT home-manager module tests..."
    @echo "Running: test-module-enabled (comprehensive module integration test)"
    nix build .#checks.{{system}}.test-module-enabled --no-link --print-build-logs
    @echo "Running: test-module-disabled"
    nix build .#checks.{{system}}.test-module-disabled --no-link --print-build-logs
    @echo "Running: test-runtime-deps-enabled"
    nix build .#checks.{{system}}.test-runtime-deps-enabled --no-link --print-build-logs
    @echo "Running: test-runtime-deps-disabled"
    nix build .#checks.{{system}}.test-runtime-deps-disabled --no-link --print-build-logs
    @echo "Running: test-daemon-disabled"
    nix build .#checks.{{system}}.test-daemon-disabled --no-link --print-build-logs

# Run runtime validation test (nixosTest with VM - slow but comprehensive)
[group('check')]
test-runtime:
    @echo "Running runtime validation test (starts VM, slower)..."
    nix build .#checks.{{system}}.test-emacs-runtime --print-build-logs

# Run tests by specific tag (smoke, fast, unit, integration, etc.)
[group('check')]
test-tag TAG:
    @echo "Running tests tagged with: {{TAG}}"
    emacs -Q --batch \
        --eval "(progn (add-to-list 'load-path \"{{config_dir}}/elisp\") (add-to-list 'load-path \"{{config_dir}}/tests\"))" \
        --eval "(require 'ert)" \
        --eval "(require 'cl-lib)" \
        --eval "(setq user-emacs-directory \"{{config_dir}}/\")" \
        --load "{{config_dir}}/tests/test-helpers.el" \
        --load "{{config_dir}}/tests/test-all.el" \
        --eval "(ert-run-tests-batch-and-exit '(tag {{TAG}}))"

# Run only unit tests (no integration tests)
[group('check')]
test-unit:
    @just test-tag unit

# Run only integration tests
[group('check')]
test-integration:
    @just test-tag integration


# Build the Emacs package with Nix
[group('build')]
build:
    @echo "Building Emacs configuration with Nix..."
    nix build .#default --print-build-logs

# Byte-compile all Emacs Lisp files (direct, for development)
[group('build')]
compile:
    #!/usr/bin/env bash
    set -euo pipefail
    # Compile in dependency order: early-init and platform first, then remaining modules
    for file in "{{config_dir}}/early-init.el" "{{config_dir}}/elisp/platform.el"; do
        echo "Compiling: $file"
        emacs -Q --batch -L "{{config_dir}}" -L "{{config_dir}}/elisp" -f batch-byte-compile "$file"
    done
    find "{{config_dir}}/elisp" -name "*.el" -not -name "platform.el" | sort | while read -r file; do
        echo "Compiling: $file"
        emacs -Q --batch -L "{{config_dir}}" -L "{{config_dir}}/elisp" \
              --load "{{config_dir}}/elisp/platform.el" \
              -f batch-byte-compile "$file"
    done
    echo "Compiling: {{config_dir}}/init.el"
    emacs -Q --batch -L "{{config_dir}}" -L "{{config_dir}}/elisp" \
          --load "{{config_dir}}/elisp/platform.el" \
          -f batch-byte-compile "{{config_dir}}/init.el"

# Maintenance and Cleanup
# ======================

# Clean byte-compiled files and cache from project directory
[group('clean')]
clean:
    @echo "Cleaning project Emacs files..."
    find "{{emacs_config_dir}}" -name "*.elc" -type f -delete 2>/dev/null || true
    find "{{emacs_config_dir}}" -name "*~" -type f -delete 2>/dev/null || true
    find "{{emacs_config_dir}}" -name "#*#" -type f -delete 2>/dev/null || true
    find "{{emacs_config_dir}}" -name ".#*" -type f -delete 2>/dev/null || true
    rm -rf "{{emacs_config_dir}}/eln-cache/" 2>/dev/null || true
    @echo "Cleaning isolated dev environment..."
    rm -rf "{{dev_home}}" 2>/dev/null || true
    @echo "Cleanup completed! (only project files, user's home config untouched)"

# Deep clean (same as clean for Nix-based config)
[group('clean')]
clean-all:
    @echo "Deep cleaning project files..."
    @echo "Note: This config uses Nix, no package directories to remove"
    just clean
    @echo "Deep cleanup completed!"

# Development Tools
# ================

# Start Emacs with project config in isolated environment (safe for testing)
[group('dev')]
emacs-dev:
    #!/usr/bin/env bash
    set -euo pipefail
    # Create isolated environment
    mkdir -p "{{dev_home}}"/{.config,.cache,.local/share}
    # Run Emacs with project config in isolation
    HOME="{{dev_home}}" \
    XDG_CONFIG_HOME="{{dev_home}}/.config" \
    XDG_CACHE_HOME="{{dev_home}}/.cache" \
    XDG_DATA_HOME="{{dev_home}}/.local/share" \
    emacs -Q \
        --eval "(progn \
                  (setq user-emacs-directory \"{{config_dir}}/\") \
                  (add-to-list 'load-path \"{{config_dir}}/elisp\") \
                  (load-file \"{{config_dir}}/init.el\"))"

# Start Emacs for interactive testing with project config (isolated)
[group('dev')]
emacs-test-interactive:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p "{{dev_home}}"/{.config,.cache,.local/share}
    echo "Starting Emacs in isolated test environment..."
    echo "HOME is temporarily set to: {{dev_home}}"
    echo "Your personal config is safe!"
    HOME="{{dev_home}}" \
    XDG_CONFIG_HOME="{{dev_home}}/.config" \
    XDG_CACHE_HOME="{{dev_home}}/.cache" \
    XDG_DATA_HOME="{{dev_home}}/.local/share" \
    emacs -Q \
        --eval "(progn \
                  (setq user-emacs-directory \"{{config_dir}}/\") \
                  (add-to-list 'load-path \"{{config_dir}}/elisp\") \
                  (load-file \"{{config_dir}}/init.el\"))"

# Start Emacs with clean configuration (no packages, no isolation - USE WITH CAUTION)
[group('dev')]
emacs-clean:
    @echo "⚠️  Warning: This runs without isolation!"
    @echo "Consider using 'just emacs-dev' instead for safe testing"
    emacs -Q --eval "(progn (add-to-list 'load-path \"{{config_dir}}/elisp\") (load-file \"{{config_dir}}/init.el\"))"

# Show configuration status
[group('info')]
info:
    @echo "Emacs Configuration Info:"
    @echo "========================"
    @echo "Config directory: {{config_dir}}"
    @echo "Install directory: {{emacs_config_dir}}"
    @echo ""
    @echo "Configuration files:"
    @find "{{config_dir}}" -name "*.el" -not -path "*/.*" | wc -l | xargs echo "  Total .el files:"
    @echo "  Main files:"
    @ls -la "{{config_dir}}"/*.el 2>/dev/null || echo "    No main .el files found"
    @echo "  Elisp modules:"
    @ls -1 "{{config_dir}}/elisp/"*.el 2>/dev/null | sed 's|.*/||; s|\.el$||' | xargs -I {} echo "    {}" || echo "    No elisp modules found"

# Show available Nix flake outputs
[group('info')]
info-nix:
    @echo "Nix Flake Outputs:"
    @echo "=================="
    nix flake show

# Show available Nix checks
[group('info')]
info-checks:
    @echo "Available Nix Checks:"
    @echo "===================="
    @echo ""
    @echo "Fast Checks:"
    @echo "  - smoke-test              (< 5 seconds, basic validation)"
    @echo ""
    @echo "ERT Unit Tests:"
    @echo "  - emacs-tests             (all ERT tests via Nix)"
    @echo "  OR use tag-based filtering:"
    @echo "  • just test-tag smoke     (smoke tests only, < 1 second)"
    @echo "  • just test-tag fast      (fast unit tests, < 5 seconds)"
    @echo "  • just test-tag unit      (unit tests only)"
    @echo "  • just test-tag TAG       (any custom tag)"
    @echo ""
    @echo "NMT Integration Tests:"
    @echo "  - test-module-enabled     (config files, aliases, service, fonts)"
    @echo "  - test-module-disabled    (module disabled behavior)"
    @echo ""
    @echo "Runtime Validation (slow, optional):"
    @echo "  • just test-runtime       (nixosTest, starts VM with actual Emacs)"
    @echo ""
    @echo "Code Quality:"
    @echo "  - formatting              (nixfmt, yamlfmt, actionlint, deadnix, statix)"
    @echo ""
    @echo "Run all checks: just test-all  or  nix flake check"
    @echo "Note: test-runtime is excluded from test-all for faster local testing"

# Update flake inputs
[group('nix')]
update:
    @echo "Updating flake inputs..."
    nix flake update

# Update specific flake input
[group('nix')]
update-input INPUT:
    @echo "Updating flake input: {{INPUT}}"
    nix flake lock --update-input {{INPUT}}

# Format Nix files
[group('nix')]
format:
    @echo "Formatting Nix files..."
    nix fmt
