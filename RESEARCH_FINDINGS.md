# Doom Emacs & Spacemacs Configuration Research Findings

**Date**: 2025-11-05
**Purpose**: Analyze Doom/Spacemacs patterns to improve jotain Emacs configuration

## Executive Summary

After analyzing Doom Emacs and Spacemacs architectures, module systems, Nix integration, and CI/testing setups, I've identified several patterns and improvements that would benefit your configuration. Key findings include advanced autoload patterns, doctor/validation scripts, improved module structure, and better testing frameworks.

---

## 1. Module Structure Comparison

### Doom Emacs Module Structure
```
modules/<category>/<module>/
├── .doommodule          # Module metadata
├── README.org           # Documentation
├── packages.el          # Package declarations
├── config.el            # Configuration
├── autoload.el          # Autoloaded functions
├── autoload/            # Directory for multiple autoload files
│   ├── foo.el
│   └── bar.el
├── doctor.el            # Diagnostic/validation
└── test/                # Module tests
    └── test-*.el
```

**Key Features:**
- **Autoload directory**: Allows splitting autoloaded functions into multiple files
- **doctor.el**: Validates module dependencies and environment
- **test/**: Module-specific tests
- **.doommodule**: Module metadata (flags, dependencies)

### Spacemacs Layer Structure
```
layers/<category>/<layer>/
├── layers.el           # Layer dependencies
├── packages.el         # Package declarations
├── funcs.el           # Helper functions
├── config.el          # Configuration
├── keybindings.el     # Keybindings (loaded last)
└── local/             # Local packages
    └── <package>/
```

**Loading Order:** `layers.el → packages.el → funcs.el → config.el → keybindings.el`

**Key Features:**
- **Explicit loading order**: Predictable, documented file loading sequence
- **funcs.el**: Dedicated file for helper functions (guards with `configuration-layer/package-used-p`)
- **keybindings.el**: Separate file for all keybindings (loaded last)
- **local/**: Support for local (non-MELPA) packages

### Current jotain Structure
```
config/
├── core.el             # Mixed: settings + packages
├── ui.el               # Mixed: UI + themes + visual
├── completion.el       # Mixed: vertico + corfu + consult
├── programming.el      # Mixed: LSP + tools + languages
├── lang-*.el          # Language-specific configs
└── ...
```

**Issues:**
- No autoload system (everything loads immediately or via hooks)
- No doctor/validation system
- No module tests
- Mixed concerns in single files

---

## 2. Advanced Autoload Patterns

### Doom's Autoload System

#### 1. **Basic Autoload Cookie**
```elisp
;;;###autoload
(defun +cc/show-caller ()
  "Show callee hierarchy using CCLS."
  (interactive)
  (ccls-call-hierarchy nil))
```

#### 2. **Autodef Cookie** (Always Available, Zero-Cost if Disabled)
```elisp
;;;###autodef
(defun +workspaces-list ()
  "Return list of workspace names."
  (if (modulep! :ui workspaces)
      (persp-names)
    nil))
```
If the module is disabled, this becomes a no-op macro.

#### 3. **Conditional Inclusion Cookie**
```elisp
;;;###if (modulep! :lang python)
;;;###autoload
(defun +python/repl ()
  "Open Python REPL."
  (interactive)
  (run-python))
```

#### 4. **Function Naming Conventions**
- `+module/command` - Interactive user commands
- `+module-helper` - Non-interactive utilities
- `+module--internal` - Private implementation details

### Recommendations for jotain

**Add to modules/*/autoload.el:**
```elisp
;;; ui/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ui/pulse-line (&rest _)
  "Pulse the current line for visual feedback."
  (pulse-momentary-highlight-one-line (point)))

;;;###autoload
(defun +ui/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (modus-themes-toggle))

;;;###autodef
(defun +ui-doom-modeline-enabled-p ()
  "Check if doom-modeline is enabled."
  (bound-and-true-p doom-modeline-mode))
```

---

## 3. Doctor/Validation System

### Doom's doctor.el Pattern

Each module can include a `doctor.el` file that validates its environment:

```elisp
;;; lang/python/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "python3")
  (warn! "Couldn't find python3 executable"))

(when (modulep! +lsp)
  (unless (or (executable-find "pyright")
              (executable-find "pylsp"))
    (warn! "Neither pyright nor pylsp found for LSP support")))

(when (modulep! +poetry)
  (unless (executable-find "poetry")
    (warn! "Poetry executable not found")))
```

### Spacemacs Equivalent

Spacemacs uses `configuration-layer/package-used-p` guards:

```elisp
;;; funcs.el
(when (configuration-layer/package-used-p 'python)
  (defun spacemacs/python-start-or-switch-repl ()
    "Start and/or switch to the REPL."
    (interactive)
    (if (executable-find "ipython")
        (run-python "ipython")
      (run-python "python3"))))
```

### Recommendation for jotain

**Create `modules/*/doctor.el` for each module:**

```elisp
;;; lang/python/doctor.el -*- lexical-binding: t; -*-
(require 'doom-doctor)  ;; Or implement simple warn! macro

(unless (executable-find "python3")
  (warn! "Python 3 executable not found"))

(when (bound-and-true-p eglot-mode)
  (unless (or (executable-find "pyright-langserver")
              (executable-find "pylsp"))
    (warn! "No Python LSP server found (pyright/pylsp)")))

(unless (file-directory-p (expand-file-name "~/.cache/python/venv"))
  (warn! "Python virtual environment directory not found"))
```

**Add to nix build:**
```nix
# Run doctor checks during build
passthru.checks.doctor = pkgs.runCommand "emacs-doctor" {} ''
  ${emacs}/bin/emacs --batch \
    --load ${./init.el} \
    --eval "(require 'doctor)" \
    --eval "(doctor-check-all)" || exit 1
  touch $out
'';
```

---

## 4. Testing Frameworks Comparison

### ERT (Emacs Regression Testing)
- **Built into Emacs 24+**
- Simple, straightforward API
- Good for unit tests
- Limited setup/teardown support

```elisp
(ert-deftest test-platform-detection ()
  :tags '(unit fast)
  (should (boundp 'platform-linux-p))
  (should (booleanp platform-linux-p)))
```

### Buttercup (BDD Framework)
- **Jasmine-inspired syntax**
- Better setup/teardown (`before-each`, `after-each`)
- Powerful spy/mock system
- Built-in CI support
- **Used by many Doom modules**

```elisp
(describe "Python module"
  :var (test-file)

  (before-each
    (setq test-file (make-temp-file "test" nil ".py")))

  (after-each
    (delete-file test-file))

  (it "should enable python-mode for .py files"
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (set-auto-mode)
      (expect major-mode :to-be 'python-mode)))

  (describe "with eglot"
    (it "should start LSP server"
      (spy-on 'eglot-ensure)
      (with-current-buffer (find-file-noselect test-file)
        (python-mode)
        (expect 'eglot-ensure :to-have-been-called)))))
```

### Current jotain Tests (ERT)
✅ Good coverage of core functionality
✅ Smoke tests for fast feedback
✅ Tagged system (`:tags '(fast unit integration)`)
⚠️ Limited mocking/spy support
⚠️ No module-specific tests

### Recommendations

**Option 1: Enhance ERT with Buttercup for complex tests**
```nix
# default.nix
emacsWithPackages (epkgs: with epkgs; [
  # ... existing packages
  buttercup  # Add for BDD-style tests
])
```

```elisp
;; tests/test-lang-python.el (buttercup style)
(describe "Python language module"
  (before-each
    (require 'lang-python))

  (describe "LSP integration"
    (it "configures pyright by default"
      (expect (assoc 'python-mode eglot-server-programs)
              :to-match "pyright"))

    (it "can fallback to pylsp"
      ;; Test configuration flexibility
      ))

  (describe "test runner integration"
    (it "runs pytest on current file"
      (spy-on 'compile :and-return-value t)
      (+python/test-file)
      (expect 'compile :to-have-been-called-with
              (rx "pytest" (+ space) "-v")))))
```

**Option 2: Keep ERT, add helpers for better mocking**
```elisp
;; tests/test-helpers.el
(defmacro with-python-buffer (file &rest body)
  "Execute BODY in a Python buffer visiting FILE."
  `(with-temp-buffer
     (setq buffer-file-name ,file)
     (python-mode)
     ,@body))

(defun mock-executable (name)
  "Mock executable NAME as available."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd name) "/mock/path"))))
    ,@body))
```

---

## 5. Doom's map! Macro Pattern

### Current jotain Approach
```elisp
;; config/keybindings.el (uses general.el)
(general-create-definer my/leader-def
  :prefix "M-SPC")

(my/leader-def
  "f" '(:ignore t :which-key "files")
  "ff" 'find-file
  "fr" 'consult-recent-file)
```

### Doom's map! Macro
```elisp
;; More concise, context-aware
(map! :leader
      (:prefix-map ("f" . "file")
       :desc "Find file" "f" #'find-file
       :desc "Recent files" "r" #'consult-recent-file)

      ;; Conditional bindings
      :when (modulep! :completion vertico)
      (:prefix "s"
       "f" #'consult-fd
       "g" #'consult-ripgrep))

;; Local mode bindings
(map! :localleader
      :map python-mode-map
      :prefix ("t" . "test")
      "t" #'+python/test-function
      "f" #'+python/test-file
      "a" #'+python/test-project)
```

### Recommendation

**Enhance general.el setup with Doom-style organization:**

```elisp
;;; modules/editor/keybindings.el

;; Leader key bindings
(my/leader-def
  :prefix "SPC"
  :keymaps 'override
  :states '(normal visual motion)

  ;; Files
  "" '(nil :which-key "leader")
  "f" '(:ignore t :which-key "files")
  "ff" '(find-file :which-key "find file")
  "fr" '(consult-recent-file :which-key "recent files")
  "fs" '(save-buffer :which-key "save file")

  ;; Buffers
  "b" '(:ignore t :which-key "buffers")
  "bb" '(consult-buffer :which-key "switch buffer")
  "bd" '(kill-current-buffer :which-key "kill buffer")

  ;; Search (only if vertico module enabled)
  ,@(when (featurep 'vertico)
      '("s" (:ignore t :which-key "search")
        "sf" (consult-fd :which-key "find file")
        "sg" (consult-ripgrep :which-key "grep"))))

;; Local leader (mode-specific) - M-SPC m
(my/local-leader-def
  :states '(normal visual)
  :keymaps 'python-mode-map
  :prefix "m"

  ;; Python-specific
  "t" '(:ignore t :which-key "test")
  "tt" '(+python/test-function :which-key "test function")
  "tf" '(+python/test-file :which-key "test file")
  "ta" '(+python/test-project :which-key "test all")
  "tc" '(+python/test-coverage :which-key "coverage")

  "r" '(:ignore t :which-key "repl")
  "rr" '(run-python :which-key "start REPL")
  "rs" '(python-shell-send-buffer :which-key "send buffer"))
```

---

## 6. Nix Integration Analysis

### Current jotain Approach ✅ EXCELLENT
Your current Nix setup is **already excellent** and in many ways superior to Doom/Spacemacs:

**Strengths:**
- ✅ Pure Nix package management (no runtime downloads)
- ✅ Flake-based with proper inputs
- ✅ Home-manager module integration
- ✅ Reproducible builds via default.nix
- ✅ Filtered deployment via config.nix (fileset)
- ✅ Multi-tier testing (ERT, NMT, VM runtime)
- ✅ Development shell isolation

### Doom/Spacemacs Nix Approaches

**1. nix-doom-emacs-unstraightened (marienz)**
- Uses Nix to build Doom packages
- Still requires Doom's package management layer
- More complex maintenance

**2. Let Doom/Spacemacs self-manage (Common approach)**
- Nix provides Emacs binary
- Doom handles all packages via straight.el
- Less reproducible

### Recommendation: Keep Current Approach

**Your approach is BETTER because:**
- Fully declarative (no runtime package downloads)
- Reproducible across systems
- Offline-capable
- Integrates with NixOS system packages
- CI-testable

**Small enhancements:**

```nix
# flake.nix - Add emacs-overlay for latest packages
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, emacs-overlay, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ emacs-overlay.overlays.default ];
      };
    in {
      # Use overlay for cutting-edge packages
      packages.x86_64-linux.emacs = pkgs.emacsWithPackagesFromUsePackage {
        config = ./init.el;
        defaultInitFile = true;
        package = pkgs.emacs-pgtk;  # Pure GTK Emacs from overlay
      };
    };
}
```

---

## 7. CI/Testing Setup Comparison

### Doom's CI (.github/workflows/)
```yaml
# .github/workflows/test.yml
name: CI
on: [push, pull_request]

jobs:
  test:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version: [27.2, 28.2, 29.1, snapshot]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v3
      - uses: doomemacs/ci@main
        with:
          emacs-version: ${{ matrix.emacs-version }}
      - name: Run tests
        run: doom ci run-tests tests/
```

**Configuration in .github/ci.el:**
```elisp
;;; .github/ci.el --- CI-specific configuration

;; Disable slow features for CI
(setq doom-modeline-icon nil)
(setq company-idle-delay nil)

;; Enable all modules for testing
(doom! :completion vertico
       :ui doom-modeline
       :lang python javascript)
```

### Current jotain CI ✅ GOOD

**Current setup:**
```yaml
# .github/workflows/test.yml
- uses: DeterminateSystems/nix-installer-action@main
- uses: DeterminateSystems/magic-nix-cache-action@main
- run: nix flake check
```

**Strengths:**
- ✅ Tests via Nix (reproducible)
- ✅ Multi-tier testing (ERT → NMT → VM)
- ✅ Caching via magic-nix-cache
- ⚠️ Only tests on one Emacs version

### Recommendations

**1. Add multi-Emacs testing:**

```nix
# flake.nix
{
  checks = let
    mkCheck = emacsPkg: pkgs.runCommand "test-${emacsPkg.name}" {} ''
      ${emacsPkg}/bin/emacs --batch \
        --load ${./tests/test-all.el} \
        --eval "(ert-run-tests-batch-and-exit)" || exit 1
      touch $out
    '';
  in {
    emacs-29-tests = mkCheck pkgs.emacs29;
    emacs-30-tests = mkCheck pkgs.emacs30;
    emacs-snapshot-tests = mkCheck pkgs.emacs-unstable;
  };
}
```

**2. Add pre-commit hooks:**

```nix
# flake.nix
{
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

  devShells.default = pkgs.mkShell {
    inherit (self.checks.pre-commit-check) shellHook;
  };

  checks.pre-commit-check = pre-commit-hooks.lib.run {
    src = ./.;
    hooks = {
      nixpkgs-fmt.enable = true;
      elisp-lint = {
        enable = true;
        entry = "${pkgs.emacs}/bin/emacs --batch -l elisp-lint.el";
      };
      ert-tests = {
        enable = true;
        entry = "just test-fast";
      };
    };
  };
}
```

---

## 8. Module-Specific Improvements

### Add README.org to Each Module

Following Doom/Spacemacs pattern:

```org
#+title: Python Language Module
#+author: jotain

* Description
This module provides Python language support with:
- LSP integration (pyright/pylsp)
- pytest integration
- Virtual environment management
- REPL support

* Features
** Language Server Protocol
Automatically starts LSP server when opening Python files.

** Test Runner
- ~M-SPC m t t~ - Test current function
- ~M-SPC m t f~ - Test current file
- ~M-SPC m t a~ - Test entire project

** Virtual Environments
Uses ~pyvenv~ for automatic venv detection.

* Configuration
#+begin_src emacs-lisp
;; Use pylsp instead of pyright
(after! python
  (setq +python-lsp-server 'pylsp))

;; Custom pytest options
(setq +python-pytest-args "-v --tb=short")
#+end_src

* Prerequisites
This module requires:
- Python 3.8+
- pyright or pylsp
- pytest (for testing)

* Troubleshooting
** LSP not starting
Check that pyright-langserver is in PATH:
#+begin_src shell
which pyright-langserver
#+end_src
```

---

## 9. Specific Recommendations Summary

### Priority 1: High Impact, Low Effort

1. **Add autoload files to existing modules** ⭐⭐⭐⭐⭐
   - Move interactive functions to `modules/*/autoload.el`
   - Use `;;;###autoload` cookies
   - Implement function naming conventions (`+module/command`)

2. **Add doctor.el validation** ⭐⭐⭐⭐
   - Create `modules/*/doctor.el` for each module
   - Validate executables, paths, dependencies
   - Integrate with nix build checks

3. **Enhance test organization** ⭐⭐⭐⭐
   - Add module-specific test files
   - Use buttercup for complex scenarios
   - Add pre-commit hooks

### Priority 2: Medium Impact, Medium Effort

4. **Complete module reorganization** ⭐⭐⭐⭐
   - Continue splitting into modules/ structure
   - Add README.org to each module
   - Implement module loader system

5. **Multi-Emacs testing** ⭐⭐⭐
   - Test against Emacs 29, 30, snapshot
   - Matrix testing in CI

6. **Keybinding consolidation** ⭐⭐⭐
   - Create `modules/editor/keybindings.el`
   - Implement local-leader patterns for all languages
   - Add which-key descriptions

### Priority 3: Nice to Have

7. **Add emacs-overlay** ⭐⭐
   - Get latest package versions
   - Access emacs-pgtk, emacs-unstable

8. **Module flags system** ⭐⭐
   - Implement conditional module features
   - E.g., `(lang/python +lsp +poetry)`

---

## 10. Implementation Roadmap

### Phase 1: Autoload System (1-2 hours)
```bash
# For each module in modules/*/
1. Create autoload.el or autoload/ directory
2. Move interactive commands (defun with (interactive))
3. Add ;;;###autoload cookies
4. Update config.el to remove moved functions
5. Create autoload generator script
```

### Phase 2: Doctor System (1 hour)
```bash
1. Create modules/*/doctor.el for critical modules
   - lang/python, lang/javascript, lang/go, tools/magit
2. Check for executables, validate paths
3. Add to nix checks
4. Update CLAUDE.md with doctor info
```

### Phase 3: Testing Enhancement (2-3 hours)
```bash
1. Add buttercup to default.nix
2. Create tests/test-lang-*.el for each language module
3. Add module-specific tests
4. Update CI to run all test tiers
5. Add pre-commit hooks
```

### Phase 4: Complete Module Reorganization (4-6 hours)
```bash
1. Finish splitting lang modules
2. Split tools/ modules (magit, lsp)
3. Create module loader (modules/init.el)
4. Update main init.el to use module system
5. Add README.org to each module
6. Test entire configuration loads correctly
```

### Phase 5: CI/Multi-Emacs (1-2 hours)
```bash
1. Add emacs-overlay to flake inputs
2. Create multi-Emacs test matrix
3. Update GitHub Actions workflow
4. Add pre-commit hooks to flake
```

---

## 11. Files to Create

### Immediate
- `modules/*/autoload.el` (for each existing module)
- `modules/*/doctor.el` (python, javascript, go, magit)
- `tests/test-lang-*.el` (buttercup-style module tests)
- `.github/ci.el` (CI-specific configuration)

### Short-term
- `modules/*/README.org` (documentation for each module)
- `modules/init.el` (module loader)
- `modules/.doommodule` (module metadata, optional)
- `lisp/autoload-generator.el` (script to generate autoloads)

---

## Conclusion

Your current Emacs configuration is **already excellent** in many areas:
- ✅ Pure Nix-based package management (better than Doom/Spacemacs)
- ✅ Multi-tier testing with fast feedback loops
- ✅ Home-manager integration
- ✅ Good use of use-package and lazy loading

**Key improvements from Doom/Spacemacs:**
1. **Autoload system** - Massive startup performance boost
2. **Doctor validation** - Better error messages and diagnostics
3. **Module-specific tests** - More maintainable test organization
4. **Better function naming** - Clearer API surface

**What NOT to adopt:**
- ❌ Doom's package management (yours is better with Nix)
- ❌ Spacemacs' "layers" terminology (your modules/ is clearer)
- ❌ Evil mode by default (not relevant if you don't use it)

The most impactful change would be implementing the autoload system, as this would provide the largest performance improvement while maintaining your excellent Nix-based architecture.
