# Data Model: Jotain Baseline

**Branch**: `001-baseline` | **Date**: 2026-02-22

## Overview

Jotain is a configuration distribution, not a data-driven application.
Its "data model" describes the structural entities that compose the
system rather than database tables or API resources. These entities
are the building blocks that features are specified against.

## Entities

### Elisp Module

A single `.el` file in `elisp/` covering one functional domain.

| Attribute | Type | Description |
|-----------|------|-------------|
| name | string | File stem (e.g., `completion`, `git`, `ui`) |
| file | path | `elisp/{name}.el` |
| purpose | string | Single-domain description |
| lexical-binding | boolean | MUST be `t` (constitution II) |
| dependencies | list[Module] | Other project modules required via `require` |
| use-package-decls | list[PackageDecl] | External packages declared via `use-package` |

**Identity**: Unique by `name`. One module per functional domain.

**Relationships**:
- Depends on zero or more other Modules (acyclic)
- Contains zero or more PackageDecls
- Loaded by `init.el` in declared order

### Package Declaration (use-package)

A `use-package` form within an Elisp Module.

| Attribute | Type | Description |
|-----------|------|-------------|
| package-name | string | Elisp package name (e.g., `vertico`, `magit`) |
| ensure | boolean/nil | `t` = Nix-managed external; `nil` = built-in |
| module | Module | Parent Elisp Module containing this declaration |

**Identity**: Unique by `(module, package-name)`.

**Relationships**:
- Belongs to exactly one Module
- If `ensure t`: resolved by `dependencies.nix` to a nixpkgs attr
- If `ensure nil`: built-in Emacs package, no Nix mapping

### Runtime Dependency

An external tool required at runtime, declared in
`nix/lib/runtime-deps.nix`.

| Attribute | Type | Description |
|-----------|------|-------------|
| name | string | Tool name (e.g., `nil`, `gopls`, `ripgrep`) |
| category | enum | `lsp-server`, `cli-tool`, `font`, `tree-sitter-grammar` |
| nix-attr | string | Nixpkgs attribute path |
| platform | list[Platform] | Platforms where available (empty = all) |
| optional | boolean | Whether it's gated by `includeRuntimeDeps` |

**Identity**: Unique by `nix-attr`.

**Relationships**:
- Used by one or more Elisp Modules at runtime
- Wrapped into the Emacs derivation via `symlinkJoin`
- Fonts installed via Home Manager `home.packages`

### Home Manager Option

A configurable parameter exposed by
`nix/modules/home/default.nix`.

| Attribute | Type | Description |
|-----------|------|-------------|
| name | string | Option path (e.g., `programs.jotain.enable`) |
| type | nix-type | `bool`, `package`, `function` |
| default | value | Default value |
| effect | string | What enabling/disabling changes |

**Current options**:

| Option | Type | Default | Effect |
|--------|------|---------|--------|
| `enable` | bool | `false` | Master switch; deploys config files |
| `enableDaemon` | bool | `true` | Systemd/launchd service; EDITOR=emacsclient |
| `extraPackages` | fn | `epkgs: []` | Additional Emacs packages |
| `includeRuntimeDeps` | bool | `true` | Install LSP, CLI, fonts to home.packages |

**Identity**: Unique by `name`.

**Relationships**:
- Each option affects the generated Home Manager configuration
- `enableDaemon` controls service files and EDITOR variable
- `includeRuntimeDeps` controls whether RuntimeDependencies are installed

### Platform

A target operating system detected at Emacs load time.

| Attribute | Type | Description |
|-----------|------|-------------|
| id | symbol | `linux`, `macos`, `android`, `windows` |
| constant | symbol | `platform-{id}-p` |
| supported | boolean | Whether it's a supported target |
| primary | boolean | Whether it's the primary development target |

**Current platforms**:

| Platform | Constant | Supported | Primary |
|----------|----------|-----------|---------|
| Linux | `platform-linux-p` | Yes | Yes |
| macOS | `platform-macos-p` | Yes | No |
| Android | `platform-android-p` | Yes | No |
| Windows | `platform-windows-p` | No (detection exists) | No |

**Relationships**:
- Gates conditional behavior in Elisp Modules
- Affects RuntimeDependency availability
- Determines service type (systemd vs launchd)

### Theme

A visual appearance configuration.

| Attribute | Type | Description |
|-----------|------|-------------|
| id | symbol | `light` or `dark` |
| emacs-theme | symbol | Actual Emacs theme name |
| variable | symbol | `jotain-theme-{id}` |
| keybinding | string | Toggle keybinding |

**Current themes**:

| ID | Emacs Theme | Variable | Toggle |
|----|-------------|----------|--------|
| light | `doom-nord-light` | `jotain-theme-light` | `C-c t` |
| dark | `doom-nord` | `jotain-theme-dark` | `C-c t` |

**Relationships**:
- Managed by `ui.el` module
- Auto-detection via `auto-dark` package
- Theme switching advised to disable all themes first (FR-010)

### Test Suite

A collection of ERT tests grouped by execution speed tag.

| Attribute | Type | Description |
|-----------|------|-------------|
| tag | symbol | `smoke`, `fast`, `unit`, `integration`, `slow` |
| max-duration | duration | Maximum allowed execution time |
| files | list[path] | Test files included in this suite |
| just-target | string | `just` command to run this suite |

**Current suites**:

| Tag | Max Duration | Just Target | Purpose |
|-----|-------------|-------------|---------|
| smoke | 1s | `just test-smoke` | Critical infrastructure validation |
| fast | 5s | `just test-fast` | Module-level unit tests |
| unit | — | `just test-unit` | Single-module tests |
| integration | — | `just test` | Cross-module + config loading |
| slow | — | `just test` | Filesystem-heavy operations |

## Entity Relationships

```text
init.el
  └─ loads ─→ [Elisp Module] ←─ contains ─→ [Package Declaration]
                    │                              │
                    │ uses at runtime               │ resolved by
                    ▼                               ▼
            [Runtime Dependency]          nix/lib/dependencies.nix
                    │
                    │ gated by
                    ▼
         [Home Manager Option]
          (includeRuntimeDeps)
                    │
                    │ affects
                    ▼
            [Platform] ←─ detects ─→ platform.el
                    │
                    │ selects
                    ▼
              [Theme] ←─ toggles ─→ ui.el + auto-dark
```

## State Transitions

### Theme State

```text
                 auto-dark detects OS change
                 ┌──────────────────────────┐
                 │                          │
                 ▼                          │
  [dark (nord)] ←──── C-c t ────→ [light (doom-nord-light)]
       │                                │
       └── disable-all-themes (advice) ─┘
```

### Daemon Lifecycle

```text
  [not running] → systemctl start emacs → [daemon running]
       │                                        │
       │                                        ├─ emacsclient -c → [frame created]
       │                                        │
       └─ emacs (direct) → [standalone frame]   └─ systemctl stop → [not running]
```
