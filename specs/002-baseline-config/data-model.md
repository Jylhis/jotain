# Data Model: Jotain Baseline Configuration

## Entities

### Module

A single `.el` file covering one functional domain.

| Attribute | Description |
|-----------|-------------|
| name | Symbol used in `(provide 'name)` and `(require 'name)` |
| directory | `lisp/` (built-in) or `modules/` (third-party) |
| dependencies | List of other modules this depends on (only `jotain-core` allowed) |
| packages | List of `use-package` declarations (`:ensure nil` for built-in, `:ensure t` for third-party) |
| provides | The symbol declared via `(provide 'module-name)` |

**Constraints:**
- Name MUST be unique across both directories
- Dependencies form a single-root DAG: only `jotain-core` may be depended upon
- Files in `lisp/` MUST NOT use `:ensure t`
- Files in `modules/` MUST use `:ensure t` and document justification

### Configuration Runtime

The running Emacs environment state.

| Attribute | Description |
|-----------|-------------|
| jotain/nix-managed-p | Boolean: t if NIX_PROFILES present, nil otherwise |
| jotain/platform | Symbol: `linux-x86_64`, `linux-aarch64`, `android-aarch64`, or `unknown` |
| jotain/emacs-version-ok | Boolean: t if Emacs >= 30 |
| user-emacs-directory | Path to the Emacs config directory |

**State transitions:**
- Set once during `early-init.el` / `jotain-core` load
- Immutable after initialization

### Home Manager Module

Nix module exposing user-facing options.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| enable | bool | false | Enable jotain |
| package | package | pkgs.jotain | The config package |
| enableDaemon | bool | true | Run Emacs as systemd service |
| includeRuntimeDeps | bool | true | Install LSP, fonts, CLI tools |
| extraPackages | function | epkgs: [] | Additional Emacs packages |

### Private Extensions

| Attribute | Description |
|-----------|-------------|
| file path | `private.el` in `user-emacs-directory` |
| load timing | After all standard modules via `after-init-hook` |
| error handling | Errors caught and reported; do not prevent startup |
