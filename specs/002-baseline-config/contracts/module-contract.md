# Module Contract: Jotain Elisp Modules

## Module File Contract

Every module file MUST conform to this contract.

### Structure

```elisp
;;; module-name.el --- One-line description -*- lexical-binding: t; -*-

;;; Commentary:
;; Multi-line description of what this module covers.

;;; Code:

;; Optional: require jotain-core (the only allowed dependency)
;; (require 'jotain-core)

;; Module configuration here...

(provide 'module-name)
;;; module-name.el ends here
```

### Rules

1. File MUST enable `lexical-binding: t` in the first line
2. File MUST end with `(provide 'module-name)` where `module-name` matches the filename without `.el`
3. File MUST be loaded via `(require 'module-name)` in `init.el`
4. File in `lisp/` MUST NOT use `use-package` with `:ensure t`
5. File in `modules/` MUST use `use-package` with `:ensure t` and include a comment justifying why the built-in alternative is insufficient
6. File in `lisp/` MAY depend on any other `lisp/` module (all assumed always available); dependencies MUST be documented and SHOULD fail gracefully
7. File in `modules/` MAY depend on anything under `lisp/` but MUST NOT depend on other `modules/` files

## Nix Integration Contract

### Package Extraction

`nix/lib/dependencies.nix` auto-extracts `(use-package PACKAGE-NAME` declarations from all `.el` files in `lisp/` and `modules/`.

**Exclusion markers** (prevent Nix installation):
- `:ensure nil` — package is built-in or provided by another mechanism
- `:nodep` — custom marker, package should not be Nix-installed
- `packageNameMap` entry with `null` value — for Emacs built-ins

### Home Manager Interface

```nix
programs.jotain = {
  enable = true;                         # Required
  package = pkgs.jotain;                 # Optional override
  enableDaemon = true;                   # Optional, default true
  includeRuntimeDeps = true;             # Optional, default true
  extraPackages = epkgs: [ epkgs.foo ];  # Optional
};
```

### Nix Overlay Interface

The overlay exposes:
- `pkgs.jotain` — The configuration package (init.el, early-init.el, lisp/, modules/)
- `pkgs.jotainEmacs` — Emacs 30 PGTK with all packages and runtime deps wrapped
