;;; early-init.el --- Pre-init hooks -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:

;; Loaded before package.el, before the GUI initialises, before init.el.
;; Anything that *must* happen before a frame is drawn or before
;; `package-initialize' runs lives here. Everything else lives in init.el
;; or one of the lisp/init-*.el modules.

;;; Code:

;; Garbage collection: bump the threshold to "effectively never" during
;; startup. `init-core' restores a sane runtime value.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Don't bother validating .elc mtimes at startup, except in batch sessions
;; where stale .elc files cause hard-to-debug surprises.
(setq load-prefer-newer noninteractive)

;; package.el stays on. Nix (and, in dev, the devenv shell) provides most
;; packages via load-path, so they load instantly without touching the
;; network. Anything Nix doesn't ship falls through to MELPA at install
;; time — see init.el for the archive list.
(setq package-quickstart t)

;; use-package is built in since Emacs 29; configure it before init.el
;; loads any `use-package` form. `always-ensure' means every block defaults
;; to `:ensure t' — built-ins must opt out with `:ensure nil'.
(setq use-package-enable-imenu-support t
      use-package-always-ensure t)

;; Disable UI chrome before the first frame is drawn — much faster than
;; toggling the modes off after the fact.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Silence the firehose of obsolete-symbol warnings from third-party
;; packages we don't control. Real warnings still surface.
(setq byte-compile-warnings '(not obsolete))

;; Don't compact font caches during GC. Trades a little RAM for noticeably
;; smoother redisplay, especially on systems with many installed fonts.
(setq inhibit-compacting-font-caches t)

;; macOS: thinner font smoothing matches the system rendering on Retina.
(when (eq system-type 'darwin)
  (setq ns-use-thin-smoothing t))

;; Native compilation (Emacs 30+): keep eln-cache out of the config dir,
;; and silence the firehose of async warnings during init.
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-speed 2)
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

;; Tree-sitter grammars provided out-of-band (Nix sets TREE_SITTER_DIR).
(when-let* ((ts-dir (getenv "TREE_SITTER_DIR")))
  (setq treesit-extra-load-path (list ts-dir)))

;; Ghostty advertises TERM=xterm-ghostty but Emacs doesn't ship a matching
;; term/xterm-ghostty.el. Aliasing to xterm-256color makes term/xterm.el
;; load instead, which restores modifyOtherKeys + 24-bit colour. Must be
;; set here because tty-run-terminal-initialization fires before init.el.
(add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm-256color"))

(provide 'early-init)
;;; early-init.el ends here
