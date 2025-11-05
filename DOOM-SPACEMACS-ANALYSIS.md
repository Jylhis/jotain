# Doom Emacs & Spacemacs Analysis: Configuration Improvements

**Date:** 2025-11-05
**Analysis of:** Doom Emacs and Spacemacs module/layer systems, Nix integration, CI/testing setups

## Executive Summary

Both Doom Emacs and Spacemacs have mature, modular architectures with valuable patterns for our configuration. However, they take fundamentally different approaches to Nix integration:

- **Doom Emacs**: Pragmatic approach - let Doom manage its own packages outside Nix
- **Spacemacs**: Similar challenges with package management conflicting with Nix
- **Our Config**: Already superior - uses Nix for reproducible package management

**Key Finding:** Our Nix-based approach is actually **more advanced** than what Doom/Spacemacs offer. We should focus on adopting their organizational patterns while keeping our Nix foundation.

---

## 1. Module/Layer Organization

### Doom Emacs Module Structure

**File Organization:**
```
modules/<category>/<module>/
├── packages.el       # Package declarations (no use-package!, just package!)
├── config.el         # Configuration (use-package!, after!, hooks)
├── autoload.el       # Lazy-loaded functions with ;;;###autoload
├── autoload/<name>.el # Additional autoload files
├── README.org        # Documentation
└── doctor.el         # Health checks (optional)
```

**Key Patterns:**

1. **packages.el** - Declarative only
   ```elisp
   ;; -*- no-byte-compile: t; -*-
   (package! pip-requirements :pin "31e0dc62...")
   (when (modulep! +cython)
     (package! cython-mode :pin "3e479055..."))
   ```
   - No side effects
   - No expensive computations
   - Module flags with `(modulep! +flag)`
   - Pinned commits for reproducibility

2. **config.el** - Configuration and setup
   ```elisp
   (use-package! python
     :mode ("\\.py\\'" . python-mode)
     :hook (python-mode . eglot-ensure)
     :config
     (setq python-indent-offset 4))
   ```
   - Uses `use-package!` (Doom wrapper)
   - Conditional blocks with `(modulep! +flag)`
   - Lazy loading with hooks/modes

3. **autoload.el** - Lazy-loaded functions
   ```elisp
   ;;;###autoload
   (defun +python/test-file ()
     "Run pytest on current file."
     (interactive)
     (compile (format "pytest -v %s" (buffer-file-name))))
   ```
   - Functions prefixed with `+module/`
   - Marked with `;;;###autoload`
   - Not loaded until called

**Module Categories:**
- `:completion` - Completion frameworks
- `:ui` - Visual enhancements
- `:editor` - Editing features
- `:emacs` - Core Emacs augmentation
- `:term` - Terminal emulation
- `:checkers` - Syntax/spell checking
- `:tools` - External tool integration
- `:lang` - Language support
- `:os` - OS-specific features
- `:app` - Complex applications (loaded last)
- `:config` - Personal config (loaded last)

### Spacemacs Layer Structure

**File Organization:**
```
layers/<category>/<layer>/
├── layers.el         # Layer dependencies (optional)
├── packages.el       # Package list and init functions
├── funcs.el          # Helper functions
├── config.el         # Layer configuration
├── keybindings.el    # Key bindings
└── local/            # Local packages
```

**Loading Order:**
1. `layers.el` → 2. `packages.el` → 3. `funcs.el` → 4. `config.el` → 5. `keybindings.el`

**Key Patterns:**

1. **packages.el** - Three-phase initialization
   ```elisp
   (defconst python-packages '(python pip-requirements))

   (defun python/pre-init-python () ...)   ; Before package loads
   (defun python/init-python () ...)       ; Main config (ownership)
   (defun python/post-init-python () ...)  ; After package loads
   ```

2. **Package Ownership**
   - Each package owned by exactly one layer's `init-` function
   - Other layers can use `pre-init-` and `post-init-` hooks
   - Only installed if at least one layer has an `init-` function

3. **Layer Dependencies**
   ```elisp
   (configuration-layer/declare-layer '(spacemacs-completion
                                        spacemacs-editing))
   ```

### Comparison: Our Current Config

**Current Structure:**
```
config/
├── core.el           # Monolithic
├── completion.el     # Monolithic
├── programming.el    # Monolithic
├── lang-python.el    # Monolithic
└── ...
```

**Issues:**
- ✗ No separation of concerns (packages mixed with config)
- ✗ No autoload files (all loaded immediately)
- ✗ No health checks or documentation
- ✗ Harder to maintain and understand
- ✓ Good: Already using lazy loading with `:defer t`

---

## 2. Nix Integration Analysis

### Doom Emacs + Nix

**Community Projects:**
- `vlaci/nix-doom-emacs` - Broken since 2024
- `marienz/nix-doom-emacs-unstraightened` - Alternative approach
- `sebnyberg/doomemacs-nix-example` - Basic flake example

**Recommended Approach (by hlissner, Doom creator):**
> "Let Doom do its thing outside of Nix"

**Why:**
- Doom uses `straight.el` for package management
- Packages are cloned from git, not installed via package.el
- Conflicts with Nix's approach of pre-installing packages
- Doom needs to manage its own lock file and dependencies

**Typical Setup:**
```nix
{
  programs.emacs.enable = true;

  home.file.".config/doom".source = ./doom.d;

  home.activation.doomEmacs = ''
    if [ ! -d "$HOME/.emacs.d" ]; then
      git clone https://github.com/doomemacs/doomemacs ~/.emacs.d
      ~/.emacs.d/bin/doom install
    fi
  '';
}
```

### Spacemacs + Nix

**Challenges:**
- Spacemacs uses its own package management system
- Cannot mix Nix-installed packages with Spacemacs package manager
- Reproducibility concerns - Spacemacs pulls packages at runtime

**Typical Setup:**
```nix
{
  programs.emacs.enable = true;

  home.file.".spacemacs".source = ./spacemacs;

  home.activation.spacemacs = ''
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
  '';
}
```

### Our Config + Nix: **Superior Approach** ✓

**Why We're Better:**
```nix
{
  # default.nix - All packages declared in Nix
  emacsWithPackages = pkgs.emacsWithPackages (epkgs: with epkgs; [
    vertico corfu consult orderless  # Completion
    magit diff-hl                     # Git
    eglot                             # LSP
    # ... 100+ packages
  ]);

  # Fully reproducible
  # Version-locked via flake.lock
  # Offline-capable
  # Build-time package installation
}
```

**Advantages:**
- ✓ **100% Reproducible** - Everything in flake.lock
- ✓ **Declarative** - All packages in default.nix
- ✓ **Offline-capable** - No runtime downloads
- ✓ **Fast** - Packages pre-installed at build time
- ✓ **Version-controlled** - Can rollback entire config
- ✓ **Nix benefits** - Binary cache, multi-user, garbage collection

**What Doom/Spacemacs Can't Do:**
- ✗ Can't guarantee reproducibility (packages pulled at runtime)
- ✗ Can't work offline (need internet for first install)
- ✗ Can't easily rollback (no atomic generations)
- ✗ Can't share binary cache

**Recommendation:** Keep our Nix approach, adopt their organizational patterns

---

## 3. Testing & CI Infrastructure

### Doom Emacs Testing

**Official CI Action:** `doomemacs/ci`

**Example Workflow:**
```yaml
name: Run tests
on: [pull_request, push]
jobs:
  run-tests:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version: [27.1, 27.2, 28.1, snapshot]
    steps:
      - uses: actions/checkout@v3.0.2
      - uses: doomemacs/ci@main
        with:
          emacs-version: ${{ matrix.emacs-version }}
      - run: doom ci run-tests tests/
```

**Features:**
- Matrix testing across OS and Emacs versions
- Caches packages by configuration hash
- Custom CI config in `.github/ci.el`
- Cross-platform support (Unix + Windows)

**Test Organization:**
- Uses Buttercup (BDD testing framework)
- Plan to add integration tests powered by Nix
- Tests in `tests/` directory

### Spacemacs Testing

**Current State:**
- Has ERT tests for core: `tests/core/core-configuration-layer-utest.el`
- Layer testing remains a challenge
- Limited CI infrastructure (only stale.yml workflow visible)

**ERT Integration:**
- Emacs-lisp layer includes ERT test runner
- Key bindings: `SPC m t b` (run buffer tests), `SPC m t q` (run ERT)
- Function: `spacemacs/ert-run-tests-buffer`

### Our Config Testing: **Industry-Leading** ✓

**Current Test Infrastructure:**

1. **ERT Unit Tests**
   ```elisp
   (ert-deftest test-platform/linux ()
     :tags '(smoke fast unit)
     (should (boundp 'platform-linux-p)))
   ```
   - Organized by tags (smoke, fast, unit, integration, slow)
   - Progressive validation tiers

2. **NMT Integration Tests**
   - Tests home-manager module deployment
   - Validates file installation, systemd services
   - Tests with actual nix builds

3. **nixosTest Runtime Validation**
   - End-to-end testing in VM
   - Tests actual Emacs execution, daemon, client connectivity
   - Real-world validation

4. **Progressive Test Tiers**
   ```bash
   just test-smoke      # < 1 second
   just test-fast       # < 5 seconds
   just test            # Full ERT suite
   just test-nmt        # Integration tests
   just test-runtime    # VM tests (~5 minutes)
   just check-all       # Everything
   ```

5. **CI Integration**
   - `nix flake check` runs tests
   - Runtime tests excluded by default (fast local dev)
   - CI=1 environment variable enables full suite

**Advantages Over Doom/Spacemacs:**
- ✓ **Multiple test levels** (unit, integration, runtime)
- ✓ **Nix-powered isolation** (reproducible test environments)
- ✓ **VM testing** (real execution validation)
- ✓ **Speed-optimized** (smoke tests < 1s)
- ✓ **Home-manager testing** (deployment validation)

**What They Don't Have:**
- ✗ No multi-tier testing strategy
- ✗ No VM/integration testing
- ✗ No deployment testing
- ✗ No Nix-based test isolation

---

## 4. Configuration Patterns to Adopt

### From Doom Emacs

#### 1. **Module Flag System**
```elisp
;; Current
(use-package python-mode ...)

;; Improved with flags
(when (modulep! +pyright)
  (use-package lsp-pyright ...))
```

**Implementation:**
```elisp
;; In init.el or custom flags system
(defvar python-use-pyright t)
(defmacro modulep! (flag)
  `(bound-and-true-p ,(intern (concat (symbol-name ',flag) "-enabled"))))
```

#### 2. **Autoload Pattern**
```elisp
;; modules/lang/python/autoload.el
;;;###autoload
(defun +python/test-file ()
  "Run pytest on current file."
  (interactive)
  (compile (format "pytest -v %s" (buffer-file-name))))

;;;###autoload
(defun +python/test-function ()
  "Run pytest on current function."
  (interactive)
  (let ((func-name (which-function)))
    (compile (format "pytest -v %s::%s"
                     (buffer-file-name)
                     func-name))))
```

**Benefits:**
- Functions not loaded until called
- Cleaner config.el (no function definitions)
- Faster startup

#### 3. **Package Pinning (Adapted for Nix)**
```nix
# default.nix - Pin specific versions
{
  emacsPackages = epkgs: with epkgs; [
    # Pin critical packages
    (magit.overrideAttrs (old: {
      src = fetchFromGitHub {
        owner = "magit";
        repo = "magit";
        rev = "v3.3.0";
        sha256 = "...";
      };
    }))
  ];
}
```

#### 4. **Module Documentation**
```org
#+TITLE: :lang python

* Description
Python language support with LSP, testing, and environment management.

* Module Flags
+ +lsp :: Enable LSP support via Eglot
+ +pyright :: Use Pyright LSP server (default: pylsp)
+ +poetry :: Enable Poetry integration

* Prerequisites
+ Python 3.8+
+ pyright or pylsp
+ pytest (for testing)
```

### From Spacemacs

#### 1. **Layer Dependencies**
```elisp
;; modules/lang/python/packages.el
;; Requires: completion, lsp
(configuration-layer/declare-dependencies '(completion tools/lsp))
```

#### 2. **Three-Phase Initialization**
```elisp
;; Pre-init: Setup before package loads
(defun +python/pre-init-eglot ()
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

;; Init: Main configuration
(defun +python/init-python-mode ()
  (use-package python
    :mode ("\\.py\\'" . python-mode)))

;; Post-init: Additional configuration
(defun +python/post-init-projectile ()
  (add-to-list 'projectile-project-root-files "pyproject.toml"))
```

#### 3. **Keybinding Files**
```elisp
;; modules/lang/python/keybindings.el
(with-eval-after-load 'python
  (spacemacs/declare-prefix-for-mode 'python-mode "mt" "test")
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "tt" #'+python/test-file
    "tf" #'+python/test-function
    "tp" #'+python/test-project))
```

---

## 5. Recommended Implementation Plan

### Phase 1: Reorganize into Doom-style Modules (IN PROGRESS)

**Structure:**
```
modules/
├── completion/
│   ├── packages.el    # Document packages (Nix installs them)
│   ├── config.el      # Configuration only
│   └── README.org     # Documentation
├── ui/
│   ├── packages.el
│   ├── config.el
│   ├── autoload.el    # +ui/pulse-line, etc.
│   └── README.org
├── editor/
│   ├── packages.el
│   ├── config.el
│   └── README.org
├── lang/
│   ├── python/
│   │   ├── packages.el
│   │   ├── config.el
│   │   ├── autoload.el  # +python/test-*, +python/run-*
│   │   └── README.org
│   ├── go/
│   ├── javascript/
│   └── ...
└── tools/
    ├── magit/
    └── lsp/
```

**Module Loader:**
```elisp
;; init.el
(defvar modules-dir (expand-file-name "modules/" user-emacs-directory))

(defun load-module (category module)
  "Load a module's config.el and autoload.el."
  (let ((module-dir (expand-file-name
                     (format "%s/%s/" category module)
                     modules-dir)))
    ;; Load autoload.el first (lazy functions)
    (let ((autoload-file (expand-file-name "autoload.el" module-dir)))
      (when (file-exists-p autoload-file)
        (load autoload-file nil t)))

    ;; Load config.el
    (let ((config-file (expand-file-name "config.el" module-dir)))
      (when (file-exists-p config-file)
        (load config-file nil t)))))

;; Load modules in order
(load-module "completion" "completion")
(load-module "ui" "ui")
(load-module "editor" "editor")
(load-module "lang" "python")
(load-module "lang" "go")
;; ...
```

### Phase 2: Add Autoload Files

**Extract functions from config.el to autoload.el:**

```elisp
;; modules/lang/python/config.el - BEFORE
(use-package python
  :config
  (defun my/python-test-file ()
    (interactive)
    (compile (format "pytest -v %s" (buffer-file-name)))))

;; modules/lang/python/config.el - AFTER (clean)
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure))

;; modules/lang/python/autoload.el - NEW
;;;###autoload
(defun +python/test-file ()
  "Run pytest on current file."
  (interactive)
  (compile (format "pytest -v %s" (buffer-file-name))))

;;;###autoload
(defun +python/test-function ()
  "Run pytest on current function."
  (interactive)
  (let ((func-name (which-function)))
    (compile (format "pytest -v %s::%s"
                     (buffer-file-name)
                     func-name))))
```

### Phase 3: Add Module Flags (Optional)

```elisp
;; config/flags.el
(defvar +python-use-pyright t)
(defvar +python-use-poetry nil)
(defvar +go-use-gopls t)

;; Convenience macro
(defmacro modulep! (flag)
  `(bound-and-true-p ,(intern (concat "+" (symbol-name ',flag)))))

;; Usage in modules/lang/python/config.el
(when (modulep! python-use-pyright)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("pyright-langserver" "--stdio")))))
```

### Phase 4: Add Documentation

```org
# modules/lang/python/README.org
#+TITLE: Python Language Module

* Description
Python development support with LSP, testing, and REPL integration.

* Features
- LSP support via Eglot (pyright or pylsp)
- pytest integration
- Virtual environment support (pyvenv, poetry)
- Import management (pyimport, py-isort)

* Keybindings
| Key         | Command                    | Description              |
|-------------+----------------------------+--------------------------|
| M-SPC m t f | +python/test-file          | Test current file        |
| M-SPC m t t | +python/test-function      | Test function at point   |
| M-SPC m t p | +python/test-project       | Test entire project      |

* Configuration
Set these variables before loading:

#+begin_src elisp
(setq +python-use-pyright t)     ; Use pyright LSP (default: nil)
(setq +python-use-poetry nil)    ; Enable poetry support
#+end_src
```

### Phase 5: Enhanced CI (Optional)

```yaml
# .github/workflows/test.yml
name: Test Configuration
on: [push, pull_request]

jobs:
  test-matrix:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
        emacs-version: ['28.2', '29.1', '30.0']
    steps:
      - uses: actions/checkout@v4

      - uses: cachix/install-nix-action@v23
        with:
          nix_path: nixpkgs=channel:nixos-unstable

      - uses: cachix/cachix-action@v12
        with:
          name: mycache
          authToken: '${{ secrets.CACHIX_AUTH_TOKEN }}'

      # Install specific Emacs version
      - name: Install Emacs ${{ matrix.emacs-version }}
        run: |
          nix-env -iA nixpkgs.emacs${{ matrix.emacs-version }}

      - name: Run smoke tests
        run: nix develop --command just test-smoke

      - name: Run fast tests
        run: nix develop --command just test-fast

      - name: Run full test suite
        if: matrix.os == 'ubuntu-latest'
        run: nix develop --command just test-all

      - name: Run runtime tests
        if: matrix.os == 'ubuntu-latest' && matrix.emacs-version == '29.1'
        run: CI=1 nix flake check
```

---

## 6. Summary: What to Adopt

### ✓ High Priority (Adopt Now)

1. **Module Organization**
   - Separate packages.el, config.el, autoload.el
   - Clear directory structure by category
   - Better maintainability

2. **Autoload Pattern**
   - Extract functions to autoload.el
   - Use `;;;###autoload` cookie
   - Faster startup

3. **Documentation**
   - README.org for each module
   - Document keybindings and features
   - Improve discoverability

### ~ Medium Priority (Consider)

4. **Module Flags**
   - Optional features (e.g., +pyright, +poetry)
   - User customization
   - Conditional loading

5. **Keybinding Files**
   - Separate keybindings.el
   - Cleaner organization
   - Easier to customize

### ✗ Low Priority / Don't Adopt

6. **Package Management**
   - ✗ Don't adopt straight.el
   - ✗ Don't adopt runtime package installation
   - ✓ Keep our Nix approach (superior)

7. **CI Infrastructure**
   - ✗ Don't replace our testing (already better)
   - ~ Consider: Multi-Emacs version testing
   - ✓ Keep our NixOS VM tests (unique advantage)

---

## 7. Key Takeaways

### Our Strengths (Keep)

1. **Nix Package Management** - More reproducible than Doom/Spacemacs
2. **Multi-Tier Testing** - More comprehensive than both
3. **Home-manager Integration** - Deployment testing they don't have
4. **VM Testing** - Real execution validation
5. **Flake-based** - Modern Nix approach

### Areas to Improve (Adopt)

1. **File Organization** - Split monolithic files
2. **Autoload Pattern** - Lazy-load functions
3. **Documentation** - Per-module README files
4. **Naming Convention** - Use `+module/function` pattern

### Don't Change

1. **Package Management** - Nix is superior
2. **Testing Infrastructure** - Already ahead
3. **Build System** - Nix flakes are modern

---

## 8. Conclusion

Our configuration is already **technically superior** to both Doom Emacs and Spacemacs in terms of:
- Reproducibility (Nix vs runtime package management)
- Testing (3-tier vs limited testing)
- Deployment (home-manager validation)

However, we can improve **organizational structure** by adopting:
- Module file separation (packages.el, config.el, autoload.el)
- Autoload pattern for lazy function loading
- Better documentation per module

**Next Steps:**
1. Complete module reorganization (already started)
2. Extract functions to autoload.el files
3. Add README.org for each module
4. Consider module flags for optional features

**DO NOT:**
- Replace Nix with straight.el
- Add runtime package installation
- Remove our testing infrastructure

Our configuration represents the **best of both worlds**: Doom/Spacemacs organizational patterns with Nix's reproducibility guarantees.
