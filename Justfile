# Justfile - Task automation for Jotain

# Default task shows available commands
default:
    @just --list

# Development environment
dev:
    nix develop

# Build the package
build:
    nix build

# Build development Emacs
build-emacs:
    nix build .#emacs-dev

# Run flake checks
check:
    nix flake check

# Update flake inputs
update:
    nix flake update

# Format all files (Nix, Shell, Elisp)
fmt:
    nix fmt

# Format alias
format: fmt

# Check formatting without modifying files (CI mode)
fmt-check:
    nix fmt -- --fail-on-change

# Run development Emacs
emacs *ARGS:
    nix develop --command emacs-dev {{ARGS}}

# Run jot CLI
jot *ARGS:
    nix develop --command jot {{ARGS}}

# Run doctor check
doctor:
    nix develop --command jot doctor

# Test - run smoke tests
test:
    @echo "Running smoke tests..."
    nix build .#checks.$(nix eval --impure --raw --expr 'builtins.currentSystem').smoke-tests

# Test in development mode
test-dev:
    @echo "Running development tests..."
    nix develop --command emacs --batch -L elisp -L elisp/core -L tests \
        --eval "(add-to-list 'load-path \"$(pwd)/elisp\")" \
        -l tests/test-helper.el \
        -l tests/test-smoke.el \
        -f ert-run-tests-batch-and-exit

# Clean build artifacts
clean:
    rm -rf result result-* .dev-home

# Clean and rebuild
rebuild: clean build

# Enter development shell with command
shell COMMAND:
    nix develop --command {{COMMAND}}

# Show package dependencies (requires elisp/ to exist)
show-deps:
    @if [ -d elisp ]; then \
        echo "Extracting dependencies from elisp files..."; \
        nix eval --impure --raw --expr '(import ./nix/lib { inherit (import <nixpkgs> {}) lib pkgs; }).dependencies.listPackageNames ./elisp'; \
    else \
        echo "elisp/ directory not found"; \
    fi
