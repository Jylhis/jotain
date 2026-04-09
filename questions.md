# Questions that needs answering


---

What is package-quickstart? @early-init.el

`package-quickstart` (Emacs 27+) pre-generates a single bytecode file from all
activated packages so `startup.el` can load them in one shot instead of scanning
package metadata on every startup. When `package-quickstart t` is set, Emacs
creates/updates `package-quickstart-file` on every `package-install` and loads it
instead of calling `package-initialize`.

This config pins `package-quickstart-file` to `var/package-quickstart.el` in
`early-init.el` because the setting must be in place *before* `startup.el`'s
automatic `package-activate-all` runs. Without the pin, the file lands at
`<user-emacs-directory>/package-quickstart.el` (outside `var/`), is never loaded,
and the byte-compile cost on every `package-install` is wasted with no startup
benefit.
