;;; completion/packages.el --- Completion packages -*- lexical-binding: t; -*-
;;; Commentary:
;; Package declarations for completion framework.
;; Packages are installed via Nix (default.nix), this file documents dependencies.
;;; Code:

;; Core completion UI
;; vertico          - Vertical completion UI
;; corfu            - In-buffer completion popup
;; consult          - Consulting completing-read
;; embark           - Contextual actions
;; embark-consult   - Embark integration with Consult

;; Completion backends and extensions
;; cape             - Completion at point extensions
;; orderless        - Flexible completion style
;; marginalia       - Rich annotations in minibuffer

;; Additional completion tools
;; kind-icon        - Completion kind icons for corfu
;; avy              - Jump to visible text
;; zoxide           - Fast directory navigation

;;; packages.el ends here
