;; Tempel templates -*- mode: lisp-data; outline-regexp: "[a-z]"; -*-
;;
;; Curated, minimal snippets for basic language constructs.  A bare
;; major-mode symbol is a section header; the (name template...) forms
;; under it apply to that mode and its derived modes.  Field markers:
;;   p    tab stop / field           n    newline
;;   n>   newline + reindent         >    reindent this line
;;   r>   region or field, reindented
;; Trigger a snippet by typing its name and completing (M-+), or insert
;; one interactively with M-*.

prog-mode

(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: " p)
(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME: " p)

emacs-lisp-mode

(defun "(defun " p " (" p ")\n" r> ")")
(let "(let ((" p "))\n" r> ")")
(when "(when " p "\n" r> ")")
(unless "(unless " p "\n" r> ")")
(dolist "(dolist (" p " " p ")\n" r> ")")

(python-mode python-ts-mode)

(for "for " p " in " p ":" n> r>)
(while "while " p ":" n> r>)
(if "if " p ":" n> r>)
(ifelse "if " p ":" n> r> n "else:" > n> q)
(def "def " p "(" p "):" n> r>)
(class "class " p ":" n> r>)
(try "try:" n> r> n "except " p ":" > n> q)
(main "if __name__ == \"__main__\":" n> r>)

(rust-mode rust-ts-mode)

(for "for " p " in " p " {" n> r> n "}" >)
(while "while " p " {" n> r> n "}" >)
(if "if " p " {" n> r> n "}" >)
(fn "fn " p "(" p ") {" n> r> n "}" >)
(match "match " p " {" n> r> n "}" >)
(impl "impl " p " {" n> r> n "}" >)
(struct "struct " p " {" n> r> n "}" >)

(go-mode go-ts-mode)

(for "for " p " {" n> r> n "}" >)
(forr "for " p ", " p " := range " p " {" n> r> n "}" >)
(if "if " p " {" n> r> n "}" >)
(func "func " p "(" p ") " p " {" n> r> n "}" >)
(switch "switch " p " {" n> r> n "}" >)
(struct "type " p " struct {" n> r> n "}" >)

(c-mode c-ts-mode)

(for "for (" p "; " p "; " p ") {" n> r> n "}" >)
(while "while (" p ") {" n> r> n "}" >)
(if "if (" p ") {" n> r> n "}" >)
(switch "switch (" p ") {" n> r> n "}" >)
(fn p " " p "(" p ") {" n> r> n "}" >)
(struct "struct " p " {" n> r> n "};" >)

(c++-mode c++-ts-mode)

(for "for (" p "; " p "; " p ") {" n> r> n "}" >)
(forr "for (auto " p " : " p ") {" n> r> n "}" >)
(while "while (" p ") {" n> r> n "}" >)
(if "if (" p ") {" n> r> n "}" >)
(switch "switch (" p ") {" n> r> n "}" >)
(fn p " " p "(" p ") {" n> r> n "}" >)
(class "class " p " {" n> "public:" n> r> n "};" >)

(typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode)

(for "for (const " p " of " p ") {" n> r> n "}" >)
(fori "for (let " p " = 0; " p " < " p "; " p "++) {" n> r> n "}" >)
(while "while (" p ") {" n> r> n "}" >)
(if "if (" p ") {" n> r> n "}" >)
(ifelse "if (" p ") {" n> r> n "} else {" > n> q n "}" >)
(fn "function " p "(" p ") {" n> r> n "}" >)
(arrow "const " p " = (" p ") => {" n> r> n "}" >)
(switch "switch (" p ") {" n> r> n "}" >)

;; Local Variables:
;; mode: lisp-data
;; End:
