(global-set-key (kbd "C-x <up>") #'other-window)
(global-set-key (kbd "C-x <down>") #'previous-window-any-frame)

;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
(use-package windmove
  :straight t
  :config
  (global-set-key (kbd "<C-s-left>")  'windmove-left)  ;; Ctrl+Cmd+left go to left window
  (global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window

  (global-set-key (kbd "<C-s-right>") 'windmove-right) ;; Ctrl+Cmd+right go to right window
  (global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window

  (global-set-key (kbd "<C-s-up>")    'windmove-up)    ;; Ctrl+Cmd+up go to upper window
  (global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+[ go to upper window

  (global-set-key (kbd "<C-s-down>")  'windmove-down)  ;; Ctrl+Cmd+down go to down window
  (global-set-key (kbd "s-}")  'windmove-down))        ;; Cmd+Shift+] got to down window


(global-set-key (kbd "M-p") 'ace-window)

(define-key function-key-map (kbd "M-]") 'event-apply-super-modifier)

;; both command keys are 'Super'
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)

;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)


;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
(setq mac-right-option-modifier 'nil)





(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)
