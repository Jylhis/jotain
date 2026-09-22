;;; showcase.el --- Screenshot scenes for Jotain  -*- lexical-binding: t; -*-

;; Loaded via `emacs --load' by the `just showcase' recipe, AFTER the normal
;; init has run, so the whole configuration is already in effect when a scene
;; is built.  Each scene arranges buffers and windows to surface one part of
;; the config, then the driver captures it in both themes via
;; `jotain-screenshot' (lisp/init-ai.el).
;;
;; Like etc/debug-init.el, this file lives OUTSIDE lisp/ and test/ on purpose:
;; the use-package scanner (nix/use-package.nix) and the elisp-compile /
;; elisp-lint flake checks only look at lisp/, so a harness here adds no CI
;; surface and contributes nothing to the Nix package set.
;;
;; Two capture facts worth knowing before editing a scene:
;;
;;   - `jotain-screenshot' cannot capture a child frame.  `x-export-frames'
;;     exports one frame's own contents, so corfu's popup is simply absent.
;;     Scenes listed in `jotain-showcase-child-frame-scenes' go through
;;     ImageMagick's `import -window root' instead, which composites every
;;     mapped window.
;;
;;   - The `popup' scene still does not show the popup.  corfu displays from
;;     `post-command-hook', which does not run when `completion-at-point' is
;;     called from a timer rather than the command loop.  Driving it through
;;     `execute-kbd-macro' is the likely fix; untested.

(require 'cl-lib)

(defvar jotain-showcase-directory
  (or (getenv "JOTAIN_SHOWCASE_DIR")
      (expand-file-name "var/showcase" user-emacs-directory))
  "Directory the showcase writes its PNGs into.")

(defvar jotain-showcase-themes '(jylhis-sheet jylhis-field)
  "Themes to capture every scene in, light first.")

(defvar jotain-showcase--failures nil
  "List of human-readable strings describing steps that degraded.")

(defun jotain-showcase--note (fmt &rest args)
  "Record and log a degraded step described by FMT and ARGS."
  (let ((msg (apply #'format fmt args)))
    (push msg jotain-showcase--failures)
    (message "jotain-showcase: %s" msg)))

(defmacro jotain-showcase--step (label &rest body)
  "Run BODY, degrading to a recorded note on error.
LABEL names the step in the log.  A scene must never abort the run: a
missing package or an unreadable file costs one window, not the capture."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error (jotain-showcase--note "%s: %S" ,label err) nil)))

(defun jotain-showcase--file (name)
  "Return NAME under `jotain-showcase-directory', creating the directory."
  (make-directory jotain-showcase-directory t)
  (expand-file-name name jotain-showcase-directory))

(defun jotain-showcase--visit (relative &optional line)
  "Visit RELATIVE inside the config directory and go to LINE.
Returns the buffer, or nil when the file is missing."
  (let ((path (expand-file-name relative user-emacs-directory)))
    (if (not (file-readable-p path))
        (progn (jotain-showcase--note "missing file %s" relative) nil)
      (let ((buf (find-file-noselect path)))
        (set-window-buffer (selected-window) buf)
        (with-current-buffer buf
          (goto-char (point-min))
          (when line (forward-line (1- line)))
          (recenter 4))
        buf))))

(defun jotain-showcase--sample-org ()
  "Return a path to a small Org file written under the showcase directory.
The repo ships no Org file, and `org-modern' needs real headings, a
table, a source block and a TODO to show what it does."
  (let ((path (jotain-showcase--file "sample.org")))
    (unless (file-exists-p path)
      (with-temp-file path
        (insert "#+title: Jotain\n"
                "#+author: Markus\n\n"
                "* Reading list\n"
                "** TODO Re-read the redisplay chapter\n"
                "   DEADLINE: <2026-08-14 Fri>\n"
                "   The gap buffer is the easy part; the redisplay engine is\n"
                "   where the interesting decisions live.\n\n"
                "** DONE Trim the startup path\n"
                "   CLOSED: [2026-07-02 Thu]\n\n"
                "* Measurements\n"
                "  | variant  | startup | closure |\n"
                "  |----------+---------+---------|\n"
                "  | mainline |   0.42s |  1.1 GB |\n"
                "  | unstable |   0.39s |  1.2 GB |\n"
                "  | igc      |   0.44s |  1.2 GB |\n\n"
                "* A source block\n"
                "  #+begin_src emacs-lisp\n"
                "    (defun jotain-var-file (name)\n"
                "      (make-directory jotain-var-dir t)\n"
                "      (expand-file-name name jotain-var-dir))\n"
                "  #+end_src\n\n"
                "* Emphasis\n"
                "  Markers are hidden until point lands on them: *bold*,\n"
                "  /italic/, =verbatim= and ~code~.\n"))
      (jotain-showcase--note "wrote sample org file (informational)"))
    path))

(defun jotain-showcase--reset ()
  "Return the frame to a single clean window between scenes."
  (jotain-showcase--step "reset"
    (when (active-minibuffer-window)
      (abort-recursive-edit))
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(defvar jotain-showcase-child-frame-scenes '(popup)
  "Scenes whose interesting UI is drawn in a child frame.
`x-export-frames' exports one frame's own contents, so a corfu popup —
a separate child frame — is simply absent from its output.  These scenes
are captured from the X root window instead, which composites every
mapped window.")

(defun jotain-showcase--capture-root (file)
  "Capture the whole X root window into FILE with ImageMagick's import.
Returns non-nil on success.  `call-process' blocks Emacs, but the child
frame stays mapped on the X server, so the popup is included."
  (and (executable-find "import")
       (eq 0 (call-process "import" nil nil nil "-window" "root" file))
       (file-exists-p file)))

(defun jotain-showcase--capture (name theme)
  "Capture the current frame as NAME under THEME.
Unlike a scene step this is deliberately NOT demoted: a capture that
fails must leave no artifact and be visible in the exit status."
  (redisplay t)
  (let ((file (jotain-showcase--file (format "%s-%s.png" name theme))))
    (if (and (memq name jotain-showcase-child-frame-scenes)
             (jotain-showcase--capture-root file))
        file
      (when (memq name jotain-showcase-child-frame-scenes)
        (jotain-showcase--note
         "%s: no `import' on PATH; child-frame UI will be missing" name))
      (jotain-screenshot file 'png))))

;;;; Scenes
;;
;; Each scene is a function of no arguments that arranges the frame.  A scene
;; whose interesting UI only exists mid-interaction takes over the capture
;; itself (see `jotain-showcase--transient-p').

(defun jotain-showcase-scene-code ()
  "A source file with the full prog-mode decoration stack."
  (jotain-showcase--visit "lisp/init-ui.el" 240)
  (jotain-showcase--step "flymake" (flymake-start)))

(defun jotain-showcase-scene-files ()
  "Dirvish over the repository, falling back to plain dired.
`init-navigation.el' configures dired itself as well as dirvish, so the
fallback still shows configured behaviour rather than stock dired."
  (let ((dir (expand-file-name "lisp" user-emacs-directory)))
    (if (fboundp 'dirvish)
        (jotain-showcase--step "dirvish" (dirvish dir))
      (jotain-showcase--note "dirvish unavailable; using dired")
      (jotain-showcase--step "dired" (dired dir)))))

(defun jotain-showcase-scene-vc ()
  "Version control: magit when available, else built-in `vc-dir' plus a diff.
`init-vc.el' configures both, so the built-in path is a real fallback
rather than a stand-in — it is what the config gives you on a build
where magit is absent."
  (if (fboundp 'magit-status-setup-buffer)
      (jotain-showcase--step "magit"
        (magit-status-setup-buffer user-emacs-directory))
    (jotain-showcase--note "magit unavailable; using built-in vc-dir")
    (jotain-showcase--step "vc-dir"
      (vc-dir user-emacs-directory))
    (jotain-showcase--step "vc diff"
      (let ((low (split-window-below)))
        (with-selected-window low
          (let ((buf (get-buffer-create "*jotain-showcase-diff*")))
            (with-current-buffer buf
              (erase-buffer)
              (insert (with-output-to-string
                        (call-process "git" nil standard-output nil
                                      "-C" (expand-file-name user-emacs-directory)
                                      "show" "--stat" "-p" "HEAD")))
              (diff-mode)
              (goto-char (point-min)))
            (set-window-buffer (selected-window) buf)))))))

(defun jotain-showcase-scene-org ()
  "An Org buffer with org-modern styling."
  (jotain-showcase--step "org"
    (let ((buf (find-file-noselect (jotain-showcase--sample-org))))
      (set-window-buffer (selected-window) buf)
      (with-current-buffer buf
        (when (fboundp 'org-mode) (org-mode))
        (goto-char (point-min))))))

(defun jotain-showcase-scene-overview ()
  "Several windows at once: the whole editor in one frame.
No tabs are created.  `tab-bar-show' is 1, so the bar stays hidden at a
single tab — which is the honest default; fabricating a second tab just
to make the bar appear would show a workspace setup that isn't in use."
  (jotain-showcase--visit "lisp/init-completion.el" 400)
  (let ((right (jotain-showcase--step "split right" (split-window-right 110))))
    (when right
      (with-selected-window right
        (jotain-showcase--step "overview dirvish"
          (dired (expand-file-name "nix" user-emacs-directory)))
        (let ((low (jotain-showcase--step "split below" (split-window-below))))
          (when low
            (with-selected-window low
              (jotain-showcase--step "overview org"
                (set-window-buffer
                 (selected-window)
                 (find-file-noselect (jotain-showcase--sample-org)))))))))))

;;;; Transient scenes
;;
;; These arrange the frame AND capture it, because the UI they exist to show
;; only lives inside a command that has not returned yet.  Each is handed the
;; capture thunk.

(defun jotain-showcase-scene-completion (capture)
  "Vertico + marginalia + orderless in the minibuffer, mid-command."
  (jotain-showcase--visit "lisp/init-prog.el" 440)
  (jotain-showcase--step "completion"
    (minibuffer-with-setup-hook
        (lambda ()
          (insert "jotain-")
          (run-at-time
           0.7 nil
           (lambda ()
             (jotain-showcase--step "completion capture" (funcall capture))
             (when (active-minibuffer-window) (abort-recursive-edit)))))
      (condition-case nil
          (call-interactively #'execute-extended-command)
        (quit nil)))))

(defun jotain-showcase-scene-popup (capture)
  "The corfu in-buffer completion popup with nerd-icons margins."
  (jotain-showcase--visit "lisp/init-core.el" 80)
  (jotain-showcase--step "popup"
    (let ((buf (get-buffer-create "*jotain-showcase-popup*")))
      (set-window-buffer (selected-window) buf)
      (select-window (get-buffer-window buf))
      (with-current-buffer buf
        (emacs-lisp-mode)
        (erase-buffer)
        ;; The prefix must be genuinely ambiguous: with a single candidate
        ;; corfu inserts it and never shows the popup.
        (insert ";; Completion is on C-M-i; the popup below is corfu.\n\n"
                "(defun jotain-showcase-demo ()\n"
                "  (jotain-")
        (goto-char (point-max)))
      ;; The popup is torn down by the next command, so show it and capture
      ;; inside the same step rather than across two timers.
      (completion-at-point)
      (funcall capture))))

(defvar jotain-showcase-scenes
  '((code       . jotain-showcase-scene-code)
    (completion . jotain-showcase-scene-completion)
    (popup      . jotain-showcase-scene-popup)
    (files      . jotain-showcase-scene-files)
    (vc         . jotain-showcase-scene-vc)
    (org        . jotain-showcase-scene-org)
    (overview   . jotain-showcase-scene-overview))
  "Alist of scene NAME to builder function, in capture order.")

(defun jotain-showcase--transient-p (fn)
  "Non-nil when scene FN captures the frame itself.
Such a scene takes the capture thunk as its single argument."
  (memq fn '(jotain-showcase-scene-completion
             jotain-showcase-scene-popup)))

;;;; Driver

(defun jotain-showcase-run ()
  "Capture every scene in `jotain-showcase-scenes' under every theme."
  (interactive)
  (setq jotain-showcase--failures nil)
  ;; auto-dark polls the system appearance and would flip the theme back
  ;; between building a scene and capturing it, silently producing two PNGs
  ;; of the same theme.
  (jotain-showcase--step "disable auto-dark"
    (when (bound-and-true-p auto-dark-mode) (auto-dark-mode -1)))
  (let ((captured 0))
    (dolist (theme jotain-showcase-themes)
      (jotain-showcase--step (format "load-theme %s" theme)
        ;; The `load-theme' advice in init-ui.el disables the previous theme,
        ;; so no manual disable-theme bookkeeping is needed here.
        (load-theme theme t))
      (dolist (entry jotain-showcase-scenes)
        (let* ((name (car entry))
               (fn (cdr entry))
               (capture (lambda ()
                          (jotain-showcase--capture name theme)
                          (cl-incf captured))))
          (jotain-showcase--reset)
          (message "jotain-showcase: %s / %s" theme name)
          (if (jotain-showcase--transient-p fn)
              (jotain-showcase--step (format "scene %s" name)
                (funcall fn capture))
            (jotain-showcase--step (format "scene %s" name) (funcall fn))
            (funcall capture)))))
    (message "jotain-showcase: wrote %d image(s) to %s"
             captured jotain-showcase-directory)
    (when jotain-showcase--failures
      (message "jotain-showcase: %d step(s) degraded:\n  %s"
               (length jotain-showcase--failures)
               (string-join (reverse jotain-showcase--failures) "\n  ")))
    captured))

(defun jotain-showcase-batch ()
  "Run the showcase and exit; non-zero when nothing was captured."
  (let ((n (condition-case err
               (jotain-showcase-run)
             (error (message "jotain-showcase: fatal: %S" err) 0))))
    (kill-emacs (if (> n 0) 0 2))))

(provide 'showcase)
;;; showcase.el ends here
