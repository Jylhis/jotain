;;; jotain-lang-registry.el --- Declarative per-language feature standard -*- lexical-binding: t; -*-

;;; Commentary:

;; Single source of truth for the per-language IDE-feature evaluation.
;; Each entry declares what the config is *meant* to wire up for one
;; language: major mode, tree-sitter grammar, LSP servers, apheleia
;; formatter, DAP adapter, snippets, inlay hints, and live-probe
;; membership.
;;
;; Readers:
;;
;;   • `jotain-lang-eval.el' (Tier-1) loads the config and compares the
;;     live wiring with each entry, then renders the capability matrix.
;;   • `jotain-lang-live.el' (Tier-2) starts a real eglot session for each
;;     `:live' entry whose server is on PATH.
;;   • `jotain-lang-doc.el' renders docs/reference/language-support.mdx.
;;   • test/lang-eval-test.el checks the table's consistency and
;;     cross-checks it against the source text of init-prog.el and the
;;     init-lang-*.el files, so drift fails the ERT suite.
;;
;; Kept outside lisp/ and test/ (like etc/debug-init.el): it is not part of
;; the loaded configuration, so the use-package scanner and the
;; elisp-compile/elisp-lint checks never see it.  Pure data plus small
;; accessors, so the ERT suite can `require' it without booting the config.

;;; Code:

(defconst jotain-lang-registry
  ;; Each entry is a plist.  Required keys:
  ;;   :id          symbol, stable identifier (and fixture directory name)
  ;;   :name        display string for the matrix
  ;;   :file        the init-lang-*.el (or init-prog.el) that owns the wiring
  ;;   :sample      basename a probe buffer is given so `set-auto-mode' fires
  ;;   :mode        major mode the sample should resolve to (grammar present)
  ;; Optional keys:
  ;;   :classic     fallback mode when the tree-sitter grammar is absent
  ;;   :grammar     treesit language symbol, or nil for non-treesit modes
  ;;   :servers     LSP server binaries eglot may reach for (first = primary);
  ;;                nil means "no LSP wired for this language today"
  ;;   :override    non-nil when the server is set by an explicit
  ;;                `eglot-server-programs' entry in init-prog.el (vs. an eglot
  ;;                built-in default).  The ERT drift check asserts these
  ;;                strictly; built-in defaults are only advisory.
  ;;   :formatter   apheleia formatter binary, or nil
  ;;   :dape        DAP adapter binary label (dlv/debugpy/codelldb), or nil
  ;;   :snippets    t when templates/jotain.eld has a section for :mode
  ;;   :inlay       t when :mode is in init-prog's inlay-hints opt-in list
  ;;   :live        t to include in the Tier-2 live LSP probe subset
  ;;   :skip-mode   t to skip strict mode-routing assertions (content- or
  ;;                filename-detected modes that are unreliable to trigger in a
  ;;                headless probe buffer); the row still appears in the matrix
  '((:id nix        :name "Nix"            :file "init-lang-nix.el"
     :sample "eval.nix"            :mode nix-ts-mode :grammar nix
     :servers ("nixd" "nil") :formatter "nixfmt" :inlay t :live t)

    (:id python     :name "Python"         :file "init-lang-python.el"
     :sample "eval.py"             :mode python-ts-mode :grammar python
     :servers ("basedpyright-langserver" "pyright-langserver" "pylsp")
     :override t :formatter "ruff" :dape "debugpy" :snippets t :inlay t :live t)

    (:id rust       :name "Rust"           :file "init-lang-rust.el"
     :sample "eval.rs"             :mode rust-ts-mode :grammar rust
     :servers ("rust-analyzer") :formatter "rustfmt" :dape "codelldb"
     :snippets t :inlay t)

    (:id go         :name "Go"             :file "init-lang-go.el"
     :sample "eval.go"             :mode go-ts-mode :grammar go
     :servers ("gopls") :override t :formatter "goimports" :dape "dlv"
     :snippets t :inlay t :live t)

    (:id typescript :name "TypeScript"     :file "init-lang-web.el"
     :sample "eval.ts"             :mode typescript-ts-mode :grammar typescript
     :servers ("typescript-language-server") :override t :formatter "prettier"
     :snippets t :inlay t :live t)

    (:id tsx        :name "TSX"            :file "init-lang-web.el"
     :sample "eval.tsx"            :mode tsx-ts-mode :grammar tsx
     :servers ("typescript-language-server") :override t :formatter "prettier"
     :snippets t :inlay t)

    (:id javascript :name "JavaScript"     :file "init-lang-web.el"
     :sample "eval.js"             :mode js-ts-mode :grammar javascript
     :servers ("typescript-language-server") :override t :formatter "prettier"
     :snippets t)

    (:id css        :name "CSS / SCSS"     :file "init-lang-web.el"
     :sample "eval.css"            :mode css-ts-mode :grammar css
     :servers ("vscode-css-language-server") :formatter "prettier")

    (:id c          :name "C"              :file "init-lang-systems.el"
     :sample "eval.c"              :mode c-ts-mode :classic c-mode :grammar c
     :servers ("clangd") :override t :formatter "clang-format" :dape "codelldb"
     :snippets t :inlay t)

    (:id c++        :name "C++"            :file "init-lang-systems.el"
     :sample "eval.cpp"            :mode c++-ts-mode :classic c++-mode :grammar cpp
     :servers ("clangd") :override t :formatter "clang-format" :dape "codelldb"
     :snippets t :inlay t)

    (:id cuda       :name "CUDA"           :file "init-lang-systems.el"
     :sample "eval.cu"             :mode cuda-ts-mode :classic c++-mode :grammar cuda
     :servers ("clangd") :override t :formatter "clang-format" :dape "codelldb"
     :snippets t :inlay t)

    (:id bash       :name "Bash / shell"   :file "init-prog.el"
     :sample "eval.sh"             :mode bash-ts-mode :classic sh-mode :grammar bash
     :servers ("bash-language-server") :formatter "shfmt" :live t)

    (:id dockerfile :name "Dockerfile"     :file "init-lang-devops.el"
     :sample "Dockerfile"          :mode dockerfile-ts-mode :classic dockerfile-mode
     :grammar dockerfile :servers ("docker-langserver") :override t :live t)

    (:id yaml       :name "YAML"           :file "init-lang-data.el"
     :sample "eval.yaml"           :mode yaml-ts-mode :classic yaml-mode :grammar yaml
     :servers ("yaml-language-server") :formatter "prettier" :live t)

    (:id json       :name "JSON"           :file "init-prog.el"
     :sample "eval.json"           :mode json-ts-mode :grammar json
     :servers ("vscode-json-language-server") :formatter "prettier")

    (:id toml       :name "TOML"           :file "init-prog.el"
     :sample "eval.toml"           :mode toml-ts-mode :classic conf-toml-mode :grammar toml
     :servers ("taplo"))

    (:id zig        :name "Zig"            :file "init-lang-systems.el"
     :sample "eval.zig"            :mode zig-ts-mode :grammar zig
     :servers ("zls") :formatter "zig" :inlay t)

    (:id ocaml      :name "OCaml"          :file "init-lang-systems.el"
     :sample "eval.ml"             :mode neocaml-mode :classic tuareg-mode :grammar ocaml
     :servers ("ocamllsp") :override t :formatter "ocamlformat" :inlay t)

    (:id haskell    :name "Haskell"        :file "init-lang-systems.el"
     :sample "eval.hs"             :mode haskell-ts-mode :classic haskell-mode :grammar haskell
     :servers ("haskell-language-server-wrapper") :inlay t)

    (:id terraform  :name "Terraform"      :file "init-lang-devops.el"
     :sample "eval.tf"             :mode terraform-mode :grammar nil
     :servers ("terraform-ls"))

    (:id cmake      :name "CMake"          :file "init-lang-systems.el"
     :sample "CMakeLists.txt"      :mode cmake-ts-mode :classic cmake-mode :grammar cmake
     :servers ("cmake-language-server"))

    (:id meson      :name "Meson"          :file "init-lang-systems.el"
     :sample "meson.build"         :mode meson-mode :grammar nil
     :formatter "meson")

    (:id qml        :name "QML"            :file "init-lang-qml.el"
     :sample "eval.qml"            :mode qml-ts-mode :grammar qmljs
     :servers ("qmlls") :override t :formatter "qmlformat")

    (:id likec4     :name "LikeC4"         :file "init-lang-devops.el"
     :sample "eval.c4"             :mode likec4-mode :grammar nil
     :servers ("likec4-lsp" "likec4") :override t)

    (:id html       :name "HTML"           :file "init-lang-web.el"
     :sample "eval.html"           :mode mhtml-ts-mode :classic web-mode :grammar html
     :servers ("vscode-html-language-server" "html-languageserver") :override t)

    (:id csv        :name "CSV"            :file "init-lang-data.el"
     :sample "eval.csv"            :mode csv-mode :grammar nil)

    (:id sql        :name "SQL"            :file "init-lang-data.el"
     :sample "eval.sql"            :mode sql-mode :grammar nil :skip-mode t)

    (:id dune       :name "Dune"           :file "init-lang-systems.el"
     :sample "dune"                :mode dune-mode :grammar nil)

    (:id bazel      :name "Bazel / Starlark" :file "init-lang-devops.el"
     :sample "BUILD.bazel"         :mode bazel-build-mode :grammar nil
     :formatter "buildifier" :skip-mode t)

    (:id structurizr :name "Structurizr DSL" :file "init-lang-devops.el"
     :sample "eval.dsl"            :mode jotain-structurizr-mode :grammar nil)

    (:id jinja2     :name "Jinja2"         :file "init-lang-data.el"
     :sample "eval.j2"             :mode jinja2-mode :grammar nil)

    (:id just       :name "Just"           :file "init-lang-devops.el"
     :sample "justfile"            :mode just-ts-mode :grammar just :skip-mode t))
  "The per-language feature standard read by the Jotain language evaluation.
See the Commentary and the inline key documentation for the plist shape.")

;;;; Accessors

(defun jotain-lang-get (entry key &optional default)
  "Return KEY from registry ENTRY, or DEFAULT (nil) when absent."
  (if (plist-member entry key) (plist-get entry key) default))

(defun jotain-lang-entry (id)
  "Return the registry entry whose :id is ID, or nil."
  (seq-find (lambda (e) (eq (plist-get e :id) id)) jotain-lang-registry))

(defun jotain-lang-live-entries ()
  "Return the registry entries flagged for the Tier-2 live probe."
  (seq-filter (lambda (e) (jotain-lang-get e :live)) jotain-lang-registry))

(provide 'jotain-lang-registry)
;;; jotain-lang-registry.el ends here
