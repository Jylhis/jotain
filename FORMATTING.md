# Code Formatting in Jotain

This project uses [treefmt](https://github.com/numtide/treefmt) for automatic code formatting across multiple languages.

## Quick Start

```bash
# Format all files in the project
just fmt
# OR
nix fmt

# Check formatting without modifying files
just fmt-check

# Check if files are formatted correctly (CI mode)
nix fmt -- --fail-on-change
```

## Configured Formatters

### Nix Files (`*.nix`)
- **Formatter**: `nixpkgs-fmt`
- **Files**: All `.nix` files in the project
- **Configuration**: Uses nixpkgs-fmt defaults

### Shell Scripts (`*.sh`)
- **Formatter**: `shfmt`
- **Files**: All `.sh` files in `cli/` directory
- **Configuration**:
  - Indent size: 2 spaces
  - Simplify code: enabled

### Emacs Lisp (`*.el`)
- **Formatter**: Emacs built-in indentation
- **Files**: All `.el` files in `elisp/` and `templates/`
- **Configuration**: Uses Emacs `elisp-mode` indentation

## Excluded Files

The following files and directories are excluded from formatting:

- `.git/` - Git metadata
- `result`, `result-*` - Nix build artifacts
- `.dev-home/` - Development environment
- `flake.lock` - Nix flake lock file
- Test artifacts in `tests/`

## CI/CD Integration

### GitHub Actions

The CI workflow (`.github/workflows/ci.yml`) automatically checks formatting on:
- All pushes to `main`, `develop`, `new-structure` branches
- All pull requests to `main`

The formatting job:
1. Checks if all files are properly formatted
2. Fails if any files need formatting
3. Provides diff output for corrections needed

### Pre-commit Hooks (Optional)

To set up local pre-commit hooks with treefmt:

```bash
# Install pre-commit in your dev shell
nix develop --command pre-commit install

# Run pre-commit on all files
nix develop --command pre-commit run --all-files
```

To enable pre-commit hooks, add to `flake.nix`:

```nix
inputs.pre-commit-hooks = {
  url = "github:cachix/pre-commit-hooks.nix";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Then configure in `perSystem`:

```nix
pre-commit.settings.hooks = {
  treefmt.enable = true;
};
```

## Manual Formatting

### Individual Files

```bash
# Format a single Nix file
nix develop --command nixpkgs-fmt file.nix

# Format a single shell script
nix develop --command shfmt -i 2 -s -w file.sh

# Format a single Emacs Lisp file
nix develop --command emacs --batch \
  -l elisp-mode \
  -f emacs-lisp-mode \
  file.el \
  -f indent-region
```

### Specific File Types

```bash
# Format only Nix files
nix develop --command find . -name "*.nix" -not -path "./result*" -exec nixpkgs-fmt {} \;

# Format only shell scripts
nix develop --command find ./cli -name "*.sh" -exec shfmt -i 2 -s -w {} \;
```

## Troubleshooting

### Formatter Not Found

If you see errors about formatters not being available:

```bash
# Update flake inputs
nix flake update

# Rebuild dev shell
nix develop --rebuild
```

### Formatting Check Fails in CI

If CI formatting check fails:

1. Run `just fmt` locally
2. Review changes with `git diff`
3. Commit formatting changes
4. Push to repository

### Custom Formatting Rules

To modify formatting rules, edit the `treefmt` section in `flake.nix`:

```nix
treefmt = {
  programs.nixpkgs-fmt.enable = true;
  
  programs.shfmt = {
    enable = true;
    indent_size = 2;  # Change this for different indentation
  };
  
  settings.global.excludes = [
    # Add files to exclude
    "path/to/exclude/**"
  ];
};
```

## Integration with Editors

### Emacs

Add to your Emacs configuration:

```elisp
;; Format on save for supported languages
(add-hook 'before-save-hook 'format-all-buffer)

;; Or use direnv + nix-shell integration
(use-package direnv
  :config
  (direnv-mode))
```

### VSCode

Install the "Nix IDE" extension and enable format-on-save:

```json
{
  "nix.enableLanguageServer": true,
  "editor.formatOnSave": true
}
```

### Vim/Neovim

```vim
" Format on save using nix fmt
autocmd BufWritePre *.nix !nix fmt %
autocmd BufWritePre *.sh !shfmt -i 2 -s -w %
```

## Resources

- [treefmt documentation](https://numtide.github.io/treefmt/)
- [treefmt-nix](https://github.com/numtide/treefmt-nix)
- [nixpkgs-fmt](https://github.com/nix-community/nixpkgs-fmt)
- [shfmt](https://github.com/mvdan/sh)
