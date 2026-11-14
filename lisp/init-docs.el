;;; init-docs.el --- Make the configuration's Info manual discoverable -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The Nix distribution (`emacsConfigPackages') wraps emacs with
;; INFOPATH pointing at emacs-config.info, so `C-h i d' already lists the configuration.
;; This module is the fallback for a host Emacs that isn't the configuration
;; wrapper: if a built manual is found (EMACS_CONFIG_INFO_DIR, or the
;; checkout's result-info/ from `nix build -f . local.packages.info-manual
;; -o projects/distros/emacs/result-info' at the repo root), add its directory to
;; `Info-additional-directory-list'.

;;; Code:

;; Not `Info-directory-list': `info-initialize' skips building it from
;; INFOPATH once it is non-nil, so seeding it would hide every other
;; manual.  Info appends `Info-additional-directory-list' afterwards.
(defvar Info-additional-directory-list)

(defvar my-info--candidate-paths
  (delq nil
        (list
         ;; Explicit override for unusual checkouts.
         (getenv "EMACS_CONFIG_INFO_DIR")
         ;; `nix build -f . local.packages.info-manual -o projects/distros/emacs/result-info'
         ;; drops the result symlink here.
         (expand-file-name "result-info/share/info" user-emacs-directory)
         ;; Fallback: `nix build .#info -o result' without a custom name.
         (expand-file-name "result/share/info" user-emacs-directory)))
  "Directories to probe for a bundled `emacs-config.info' file.")

(let ((found
       (seq-find
        (lambda (dir)
          (and (stringp dir)
               (file-exists-p (expand-file-name "emacs-config.info" dir))))
        my-info--candidate-paths)))
  (when found
    (with-eval-after-load 'info
      (add-to-list 'Info-additional-directory-list found)))
  (when (and init-file-debug (not found))
    (message "init-docs: no emacs-config.info found in %S"
             my-info--candidate-paths)))

(provide 'init-docs)
;;; init-docs.el ends here
