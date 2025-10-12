config_dir := justfile_directory()
emacs_config_dir := env_var_or_default('XDG_CONFIG_HOME', env_var('HOME') + '/.config') + '/emacs'
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

# Run ERT unit tests via Nix
[group('check')]
test:
    @echo "Running ERT unit tests via Nix..."
    nix build .#checks.{{system}}.emacs-tests --print-build-logs

# Run all checks including NMT tests
[group('check')]
test-all:
    @echo "Running all tests (ERT + NMT)..."
    nix flake check --print-build-logs

# Run NMT home-manager module tests
[group('check')]
test-nmt:
    @echo "Running NMT home-manager module tests..."
    @echo "Running: test-emacs-config-files"
    nix build .#checks.{{system}}.test-emacs-config-files --print-build-logs
    @echo "Running: test-shell-aliases"
    nix build .#checks.{{system}}.test-shell-aliases --print-build-logs
    @echo "Running: test-emacs-service"
    nix build .#checks.{{system}}.test-emacs-service --print-build-logs
    @echo "Running: test-font-packages"
    nix build .#checks.{{system}}.test-font-packages --print-build-logs
    @echo "Running: test-module-disabled"
    nix build .#checks.{{system}}.test-module-disabled --print-build-logs
    @echo "Running: test-fileset-source"
    nix build .#checks.{{system}}.test-fileset-source --print-build-logs

# Run unit tests with verbose output (legacy direct execution)
[group('check')]
test-verbose:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Running ERT unit tests (verbose, direct execution)..."
    emacs -Q --batch \
        --eval "(progn (add-to-list 'load-path \"{{config_dir}}/lisp\") (add-to-list 'load-path \"{{config_dir}}/tests\") (add-to-list 'load-path \"{{config_dir}}/config\"))" \
        --eval "(require 'ert)" \
        --eval "(require 'cl-lib)" \
        --load "{{config_dir}}/tests/test-utils.el" \
        --load "{{config_dir}}/tests/test-platform.el" \
        --load "{{config_dir}}/tests/test-auth-source-1password.el" \
        --eval "(ert-run-tests-batch-and-exit t)"

# Run elisp-lint on all Emacs Lisp files
[group('check')]
lint:
    #!/usr/bin/env bash
    set -euo pipefail
    find "{{config_dir}}" -name "*.el" -not -path "*/.*" | while read -r file; do
        echo "Linting: $file"
        emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch "$file"
    done

# Run elisp-lint on a specific file
[group('check')]
lint-file FILE:
    emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch "{{FILE}}"

# Check for package-lint issues
[group('check')]
package-lint:
    #!/usr/bin/env bash
    set -euo pipefail
    find "{{config_dir}}" -name "*.el" -not -path "*/.*" | while read -r file; do
        echo "Package-linting: $file"
        emacs -Q --batch -l package-lint.el -f package-lint-batch-and-exit "$file"
    done

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
    find "{{config_dir}}" -name "*.el" -not -path "*/.*" | while read -r file; do
        echo "Compiling: $file"
        emacs -Q --batch -L "{{config_dir}}" -L "{{config_dir}}/config" -L "{{config_dir}}/lisp" -f batch-byte-compile "$file"
    done

# Maintenance and Cleanup
# ======================

# Clean byte-compiled files and cache
[group('clean')]
clean:
    @echo "Cleaning Emacs configuration..."
    find "{{emacs_config_dir}}" -name "*.elc" -type f -delete 2>/dev/null || true
    find "{{emacs_config_dir}}" -name "*~" -type f -delete 2>/dev/null || true
    find "{{emacs_config_dir}}" -name "#*#" -type f -delete 2>/dev/null || true
    find "{{emacs_config_dir}}" -name ".#*" -type f -delete 2>/dev/null || true
    rm -rf "{{emacs_config_dir}}/eln-cache/" 2>/dev/null || true
    @echo "Cleanup completed!"

# Clean and remove all package directories
[group('clean')]
clean-all:
    @echo "Deep cleaning Emacs configuration..."
    just clean
    rm -rf "{{emacs_config_dir}}"/{elpa,elpaca,straight,.packages,quelpa}/ 2>/dev/null || true
    rm -f "{{emacs_config_dir}}"/package-quickstart.el 2>/dev/null || true
    rm -f "{{emacs_config_dir}}"/session.* 2>/dev/null || true
    rm -f "{{emacs_config_dir}}"/desktop 2>/dev/null || true
    @echo "Deep cleanup completed!"

# Development Tools
# ================

# Start Emacs with clean configuration (no packages)
[group('dev')]
emacs-clean:
    emacs -Q --eval "(progn (add-to-list 'load-path \"{{config_dir}}/config\") (add-to-list 'load-path \"{{config_dir}}/lisp\") (load-file \"{{config_dir}}/init.el\"))"

# Start Emacs daemon
[group('dev')]
daemon:
    emacs --daemon

# Connect to Emacs daemon
[group('dev')]
client:
    emacsclient -c -a emacs

# Test configuration with minimal setup
[group('dev')]
test-minimal:
    emacs -Q --batch --eval "(message \"Emacs version: %s\" emacs-version)"

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
    @echo "  Config modules:"
    @ls -1 "{{config_dir}}/config/"*.el 2>/dev/null | sed 's|.*/||; s|\.el$||' | xargs -I {} echo "    {}"
    @echo "  Utility modules:"
    @ls -1 "{{config_dir}}/lisp/"*.el 2>/dev/null | sed 's|.*/||; s|\.el$||' | xargs -I {} echo "    {}" || echo "    No utility modules found"

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
    @echo "ERT Tests:"
    @echo "  - emacs-tests"
    @echo ""
    @echo "NMT Tests:"
    @echo "  - test-emacs-config-files"
    @echo "  - test-shell-aliases"
    @echo "  - test-emacs-service"
    @echo "  - test-font-packages"
    @echo "  - test-module-disabled"
    @echo "  - test-fileset-source"
    @echo ""
    @echo "Other Checks:"
    @echo "  - formatting"

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
