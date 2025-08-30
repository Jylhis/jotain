# Platform Support System

This document describes the platform support system for Emacs configuration, which enables platform-specific adaptations for Android, macOS, Linux, and Windows.

## Overview

The platform support system consists of:

- **Platform Detection** (`lisp/platform.el`): Core detection and utilities
- **Platform Configurations** (`config/platforms.el`): General platform adaptations
- **Specialized Configs** (`config/android.el`): Platform-specific enhancements
- **Examples** (`config/platform-examples.el`): Usage patterns and examples

## Platform Detection

### Constants

```elisp
platform-android-p     ; Non-nil on Android/Termux
platform-macos-p       ; Non-nil on macOS
platform-linux-p       ; Non-nil on Linux (excluding Android)
platform-windows-p     ; Non-nil on Windows
platform-gui-p         ; Non-nil in GUI mode
platform-terminal-p    ; Non-nil in terminal mode
```

### Feature Detection

```elisp
(platform-has-feature-p 'notifications)  ; Platform supports notifications
(platform-has-feature-p 'clipboard)      ; Clipboard integration available
(platform-has-feature-p 'fonts)          ; Font rendering support
(platform-has-feature-p 'native-comp)    ; Native compilation available
(platform-has-feature-p 'treesitter)     ; Tree-sitter support
```

### Conditional Macros

```elisp
(platform-when platform-android-p
  ;; Android-specific code
  (setq some-android-setting t))

(platform-unless platform-windows-p
  ;; Code for non-Windows platforms
  (use-package unix-specific-package))

(platform-cond
  (platform-android-p (message "Running on Android"))
  (platform-macos-p   (message "Running on macOS"))
  (t                   (message "Running on other platform")))
```

## Usage Patterns

### Conditional Package Loading

```elisp
;; Only load on GUI systems
(platform-when platform-gui-p
  (use-package pdf-tools :ensure))

;; Skip problematic packages on Android
(platform-unless platform-android-p
  (use-package vterm :ensure))

;; Platform-specific alternatives
(platform-cond
  (platform-android-p (use-package simple-package :ensure))
  (t                   (use-package full-featured-package :ensure)))
```

### Platform-Specific Configuration

```elisp
(use-package magit
  :ensure
  :config
  ;; Base configuration for all platforms
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; Platform-specific optimizations
  (platform-when platform-android-p
    (setq magit-diff-refine-hunk nil)) ; Performance optimization

  (platform-when platform-macos-p
    (setq magit-git-executable "/usr/local/bin/git"))) ; macOS path
```

### Platform-Aware Keybindings

```elisp
;; Universal bindings
(global-set-key (kbd "M-o") 'other-window)

;; Platform-specific bindings
(platform-when platform-macos-p
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank))

(platform-when platform-android-p
  (global-set-key (kbd "C-<tab>") 'other-window))
```

## Android Support

### Hardware Integration

The Android configuration (`config/android.el`) provides:

- **Volume Key Modifiers**: Use volume keys as Control/Meta modifiers
- **Touch Optimization**: Larger fonts, visual line mode, smooth scrolling
- **Storage Integration**: Automatic external storage detection
- **Custom Menu**: Touch-friendly menu system

### Android-Specific Features

```elisp
(android-phone-p)           ; Detect phone vs tablet
(android-show-platform-info) ; Display platform information
(android-restart-emacs)     ; Restart Emacs in Termux
```

### Disabled Packages

Packages automatically disabled on Android:

- `vterm` (terminal issues)
- `pdf-tools` (rendering problems)
- `exwm` (X11 window manager)
- `magit-delta` (performance)
- `all-the-icons-dired` (rendering issues)

## macOS Support

### System Integration

```elisp
;; Proper key handling
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Native features
(setq ns-use-native-fullscreen t)
(setq delete-by-moving-to-trash t)

;; System shortcuts
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
```

## Linux Support

### X11 Integration

```elisp
;; Clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; Browser integration
(setq browse-url-browser-function 'browse-url-xdg-open)
```

## Windows Support

### Windows-Specific Settings

```elisp
;; Windows key handling
(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'super)

;; Line endings
(setq-default buffer-file-coding-system 'utf-8-unix)
```

## Performance Tuning

### Platform-Specific Optimizations

```elisp
(platform-cond
  ;; Android: Aggressive optimization
  (platform-android-p
   (setq gc-cons-threshold (* 20 1024 1024))
   (setq auto-save-default nil))

  ;; Desktop: Balanced settings
  (t
   (setq gc-cons-threshold (* 2 1024 1024))))
```

## Testing

Run platform detection tests:

```elisp
M-x ert RET test-platform RET
```

## Extending Platform Support

### Adding New Platforms

1. Add detection logic to `lisp/platform.el`
2. Create platform-specific configuration file
3. Update `config/platforms.el` to load new platform
4. Add tests in `tests/test-platform.el`

### Adding Platform Features

1. Add feature detection to `platform-has-feature-p`
2. Document in this file
3. Add examples to `config/platform-examples.el`

## File Structure

```
sources/emacs/
├── lisp/
│   └── platform.el          # Core platform detection
├── config/
│   ├── platforms.el         # General platform adaptations
│   ├── android.el           # Android-specific configuration
│   ├── platform-examples.el # Usage examples
│   └── ...
├── tests/
│   └── test-platform.el     # Platform tests
└── PLATFORM.md             # This documentation
```

## Integration

The platform system is automatically loaded in `init.el`:

```elisp
;; Load platform detection first
(require 'platform)

;; Load configuration modules...
(require 'core)
(require 'ui)
;; ...

;; Load platform-specific configurations
(require 'platforms)
(when platform-android-p (require 'android))
```

This ensures platform detection is available to all configuration modules.
