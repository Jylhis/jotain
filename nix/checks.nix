# nix/checks.nix — Flake check derivations for Jotain.
#
# Application and configuration checks; dev-shell assertions live in
# devenv.nix enterTest.
{
  pkgs,
  src,
  treefmtCheck,
}:
let
  inherit (pkgs) lib;
  inherit (lib) fileset;

  # Narrowed check sources
  #
  # lib.fileset needs a real path, not the string-like flake `src', so
  # these are built from ../. (the same tree; inside a flake it is the
  # store copy, without .git or ignored files).
  #
  # `src' stays a parameter for options-doc.nix, whose `srcPrefix' strip
  # turns declaration paths into GitHub URLs; a fileset source would
  # break it.
  repoRoot = ../.;
  elIn = dir: fileset.fileFilter (f: lib.hasSuffix ".el" f.name) (repoRoot + dir);

  configFiles = fileset.unions [
    (repoRoot + "/early-init.el")
    (repoRoot + "/init.el")
    (elIn "/lisp")
  ];
  # elisp-lint and elisp-test also walk test/; elisp-compile does not.
  elispSrc = fileset.toSource {
    root = repoRoot;
    fileset = fileset.union configFiles (elIn "/test");
  };
  # elisp-test also needs etc/lang-eval/ and templates/ (read by
  # test/lang-eval-test.el). The checks that never load tests keep the
  # narrower elispSrc and its cache key.
  elispTestSrc = fileset.toSource {
    root = repoRoot;
    fileset = fileset.unions [
      configFiles
      (elIn "/test")
      (elIn "/etc/lang-eval")
      (repoRoot + "/templates")
    ];
  };
  nixSrc = fileset.toSource {
    root = repoRoot;
    fileset = fileset.fileFilter (f: lib.hasSuffix ".nix" f.name) repoRoot;
  };

  # The inner emacsWithPackages result (`passthru.core'), not the outer
  # wrapper: the wrapper's INFOPATH pulls in jotainInfo, so docs/@doc
  # edits would invalidate these checks. `core' moves only when the
  # package set does (the lisp/ scan reads package names only).
  elispEmacs = pkgs.jotainEmacsPackages.core;

  hmStubModule = {
    options = {
      assertions = lib.mkOption {
        type = lib.types.listOf lib.types.unspecified;
        default = [ ];
      };
      home.sessionVariables = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
      };
      home.packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
      };
      xdg.configHome = lib.mkOption {
        type = lib.types.str;
        default = "/tmp/jotain-home/.config";
      };
      xdg.configFile = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      systemd.user.services = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      systemd.user.sockets = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      launchd.agents = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      programs = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      fonts = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
    };
  };

  evalHomeModule =
    jotainConfig:
    lib.evalModules {
      modules = [
        hmStubModule
        ../module.nix
        {
          services.jotain = {
            enable = true;
          }
          // jotainConfig;
        }
      ];
      specialArgs = { inherit pkgs; };
    };

  defaultModule = evalHomeModule { };
  graphicalModule = evalHomeModule {
    startWithUserSession = "graphical";
  };

  # Minimal stand-in for the nix-on-droid module system (eval only).
  nixOnDroidStubModule = {
    options = {
      assertions = lib.mkOption {
        type = lib.types.listOf lib.types.unspecified;
        default = [ ];
      };
      environment.packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
      };
      environment.sessionVariables = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
      };
    };
  };

  evalNixOnDroidModule =
    jotainConfig:
    lib.evalModules {
      modules = [
        nixOnDroidStubModule
        ../module-nix-on-droid.nix
        {
          services.jotain = {
            enable = true;
          }
          // jotainConfig;
        }
      ];
      specialArgs = { inherit pkgs; };
    };

  nixOnDroidModule = evalNixOnDroidModule { };
in
{
  packages-default = pkgs.jotainEmacsPackages;
  packages-emacs = pkgs.jotainEmacs;
  packages-info = pkgs.jotainInfo;

  options-doc = import ./options-doc.nix { inherit pkgs src; };

  packages-doc = import ./packages-doc.nix { inherit pkgs src; };

  # Build-only (the output is not checked in). Heavy, so deploy-path
  # only: PR CI's subset skips it and its `site` job builds
  # `.#site-preview`, which omits it.
  emacs-api-doc = import ./emacs-api-doc.nix { inherit pkgs src; };

  # docs/configuration/package-reference.mdx is checked in so Mintlify
  # can serve it without Nix; it must match nix/packages-doc.nix byte
  # for byte.
  packages-doc-in-sync =
    let
      generated = import ./packages-doc.nix { inherit pkgs src; };
    in
    pkgs.runCommandLocal "check-packages-doc-in-sync"
      {
        trackedMdx = repoRoot + "/docs/configuration/package-reference.mdx";
        generatedMdx = "${generated}/package-reference.mdx";
      }
      ''
        if ! diff -u "$trackedMdx" "$generatedMdx"; then
          echo "" >&2
          echo "docs/configuration/package-reference.mdx is out of sync with" >&2
          echo "the ;;; @doc markers in lisp/init-*.el." >&2
          echo "Refresh it with: just docs-refresh-packages" >&2
          exit 1
        fi
        touch $out
      '';

  # docs/reference/language-support.mdx must match a fresh render of
  # etc/lang-eval/jotain-lang-registry.el. Cheap: registry only.
  inherit ((import ./lang-eval.nix { inherit pkgs; })) lang-eval-doc-in-sync;

  # config/eca/config.json and gptel's OpenRouter :models
  # (lisp/init-ai.el) are hand-kept copies of one list. The elisp side is
  # read with Emacs' reader, anchored on the OpenRouter form (not the
  # Ollama :models further down).
  eca-models-in-sync =
    pkgs.runCommand "check-eca-models-in-sync"
      {
        nativeBuildInputs = [
          pkgs.jq
          elispEmacs
        ];
        ecaConfig = repoRoot + "/config/eca/config.json";
        initAi = repoRoot + "/lisp/init-ai.el";
      }
      ''
        jsonModels=$(jq -r '.providers.openrouter.models | keys[]' "$ecaConfig" | sort)

        cat > extract.el <<'EOF'
        ;;; extract.el --- read gptel :models -*- lexical-binding: t; -*-
        (with-temp-buffer
          (insert-file-contents (getenv "INIT_AI"))
          (goto-char (point-min))
          (re-search-forward "gptel-make-openai")
          (re-search-forward ":models[[:space:]]*'")
          (dolist (m (read (current-buffer)))
            (princ (format "%s\n" m))))
        EOF
        elispModels=$(INIT_AI="$initAi" emacs -Q --batch -l extract.el | sort)

        if [ "$jsonModels" != "$elispModels" ]; then
          echo "" >&2
          echo "config/eca/config.json and lisp/init-ai.el disagree on the" >&2
          echo "OpenRouter model list (they are hand-kept copies of one" >&2
          echo "catalogue). Reconcile both. Diff (< json, > elisp):" >&2
          diff <(echo "$jsonModels") <(echo "$elispModels") >&2 || true
          exit 1
        fi
        touch $out
      '';

  # website/public/ds is committed (website/public needs no build step)
  # and must match nix/design-pin.nix, which the Emacs themes also use.
  ds-in-sync =
    let
      dsAssets = import ./ds-assets.nix {
        inherit pkgs;
        inherit (pkgs) bun;
      };
    in
    pkgs.runCommandLocal "check-ds-in-sync"
      {
        inherit dsAssets;
        vendoredDs = repoRoot + "/website/public/ds";
      }
      ''
        if ! diff -r "$dsAssets" "$vendoredDs"; then
          echo "" >&2
          echo "website/public/ds is out of sync with the jylhis/design rev" >&2
          echo "pinned in nix/design-pin.nix." >&2
          echo "Re-vendor it with: just ds-sync" >&2
          exit 1
        fi
        touch $out
      '';

  # flake.lock and devenv.lock must pin the same shared revs (Dependabot
  # bumps flake.lock alone). Without this, only PR CI's `just verify`
  # (same script) guards drift; this also covers pushes to main/next.
  locks-in-sync =
    pkgs.runCommandLocal "check-locks-in-sync"
      {
        inherit src;
        nativeBuildInputs = [ pkgs.jq ];
      }
      ''
        # Checked separately so an untracked/renamed script reports
        # itself instead of masquerading as lock drift.
        if [ ! -f "$src/scripts/verify-locks.sh" ]; then
          echo "scripts/verify-locks.sh is missing from the flake source." >&2
          echo "If you just created it, git add it — the flake source is" >&2
          echo "tracked files only." >&2
          exit 1
        fi
        if ! bash "$src/scripts/verify-locks.sh" "$src"; then
          echo "" >&2
          echo "flake.lock and devenv.lock disagree on a shared input's rev." >&2
          echo "Re-sync them with: just sync-devenv" >&2
          exit 1
        fi
        touch $out
      '';

  module-eval =
    pkgs.runCommandLocal "check-module-eval"
      {
        defaultEditorConfigured = if defaultModule.config.home.sessionVariables ? EDITOR then "1" else "0";
        graphicalTarget =
          if pkgs.stdenv.hostPlatform.isLinux then
            builtins.toJSON graphicalModule.config.systemd.user.services.jotain.Install.WantedBy
          else
            toString (builtins.length graphicalModule.config.launchd.agents.jotain.config.ProgramArguments);
      }
      ''
        touch $out
      '';

  # Eval-only: the module evaluates and sets EDITOR.
  nix-on-droid-module-eval =
    pkgs.runCommandLocal "check-nix-on-droid-module-eval"
      {
        editorConfigured =
          if nixOnDroidModule.config.environment.sessionVariables ? EDITOR then "1" else "0";
        packageCount = toString (builtins.length nixOnDroidModule.config.environment.packages);
      }
      ''
        test "$editorConfigured" = "1" || { echo "EDITOR not set"; exit 1; }
        test "$packageCount" -ge 1 || { echo "no packages installed"; exit 1; }
        touch $out
      '';

  # The dev shell has no Emacs, so jotainEmacs is checked here: expected
  # binaries exist and run without touching paths outside the store.
  emacs-binaries =
    pkgs.runCommandLocal "check-emacs-binaries"
      {
        emacs = pkgs.jotainEmacs;
      }
      ''
        set -euo pipefail
        for bin in emacs emacsclient etags; do
          test -x "$emacs/bin/$bin" || { echo "missing $bin"; exit 1; }
        done
        "$emacs/bin/emacs" --batch --version | grep -q "GNU Emacs"

        isolated_home="$(mktemp -d)"
        HOME="$isolated_home" "$emacs/bin/emacs" --batch \
          --no-init-file --no-site-file \
          --eval '(princ (format "user-init-file=%S\n" user-init-file))' \
          --eval '(princ (format "user-emacs-directory=%S\n" user-emacs-directory))' \
          > "$isolated_home/out" 2> "$isolated_home/err"
        host_leaks=$(grep -E "/home/|/Users/" \
          "$isolated_home/out" "$isolated_home/err" \
          | grep -v "$isolated_home" || true)
        if [ -n "$host_leaks" ]; then
          echo "FAIL: emacs touched a path outside the store:"
          echo "$host_leaks"
          exit 1
        fi
        if [ -e "$isolated_home/.emacs.d" ] || [ -e "$isolated_home/.emacs" ]; then
          echo "FAIL: emacs created config under HOME=$isolated_home"
          exit 1
        fi

        touch $out
      '';

  formatting = treefmtCheck;

  statix =
    pkgs.runCommandLocal "check-statix"
      {
        nativeBuildInputs = [ pkgs.statix ];
        src = nixSrc;
      }
      ''
        cd $src
        statix check .
        touch $out
      '';

  deadnix =
    pkgs.runCommandLocal "check-deadnix"
      {
        nativeBuildInputs = [ pkgs.deadnix ];
        src = nixSrc;
      }
      ''
        cd $src
        deadnix --fail .
        touch $out
      '';

  # Elisp syntax (balanced parens)
  #
  # The Emacs-based checks use `runCommand', not `runCommandLocal' (which
  # sets allowSubstitutes = false): substituting the cached result from
  # jylhis cachix beats rebuilding against a multi-hundred-MB Emacs
  # closure, so a PR touching neither lisp/ nor test/ never pulls Emacs.
  # They read no /proc, HOME, PATH or network, so remote builders are fine.
  elisp-lint =
    pkgs.runCommand "check-elisp-lint"
      {
        nativeBuildInputs = [ pkgs.jotainEmacs ];
        src = elispSrc;
      }
      ''
        cd $src
        emacs -Q --batch --eval '
          (let ((files (append (list "early-init.el" "init.el")
                               (directory-files "lisp" t "^\\(init-.*\\|devenv\\)\\.el$")
                               (directory-files "test" t "\\.el$")))
                (failed nil))
            ;; Guard against a narrowed fileset that silently lost
            ;; lisp/ or test/ and would pass over nothing.
            (when (< (length files) 30)
              (message "FAIL: only %d files found — narrowed source is wrong"
                       (length files))
              (kill-emacs 1))
            (dolist (f files)
              (condition-case err
                  (with-temp-buffer
                    (insert-file-contents f)
                    (emacs-lisp-mode)
                    (check-parens)
                    (message "OK: %s" (file-name-nondirectory f)))
                (error
                 (message "FAIL %s: %S" (file-name-nondirectory f) err)
                 (setq failed t))))
            (when failed (kill-emacs 1)))'
        touch $out
      '';

  # Regex scanner fidelity vs the Emacs reader
  #
  # nix/use-package.nix finds `(use-package NAME' by regex, so a
  # commented-out, quoted or string-embedded occurrence could be
  # miscounted (or a real form missed). Diff its output against the
  # use-package heads Emacs' reader finds in lisp/.
  scanner-fidelity =
    let
      usePackage = import ./use-package.nix { inherit lib; };
      scannerNames = lib.sort (a: b: a < b) (
        lib.unique (lib.concatMap (f: map (e: e.name) f.entries) (usePackage.scanDirectoryWithDoc ../lisp))
      );
      scannerList = pkgs.writeText "scanner-use-package-names" (
        lib.concatStringsSep "\n" scannerNames + "\n"
      );
    in
    pkgs.runCommand "check-scanner-fidelity"
      {
        nativeBuildInputs = [ elispEmacs ];
        src = elispSrc;
        inherit scannerList;
      }
      ''
        # Stay in the writable build dir; $src is a read-only store path.
        cat > collect.el <<'EOF'
        ;;; collect.el --- reader-truth use-package heads -*- lexical-binding: t; -*-
        (require 'cl-lib)
        (let ((names '())
              (dir (expand-file-name "lisp" (getenv "SRC"))))
          (cl-labels ((walk (form)
                        (when (consp form)
                          (when (and (eq (car form) 'use-package)
                                     (symbolp (car-safe (cdr form)))
                                     (cadr form)
                                     (not (memq :disabled form)))
                            (push (symbol-name (cadr form)) names))
                          ;; cdr-walk (not dolist): forms contain dotted
                          ;; pairs (alist entries like ("k" . cmd)).
                          (let ((tail form))
                            (while (consp tail)
                              (walk (car tail))
                              (setq tail (cdr tail)))))))
            (dolist (file (directory-files-recursively dir "\\.el\\'"))
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (condition-case nil
                    (while t (walk (read (current-buffer))))
                  (end-of-file nil)))))
          (dolist (n (sort (delete-dups names) #'string<))
            (princ (format "%s\n" n))))
        EOF
        SRC="$src" emacs -Q --batch -l collect.el > reader-names.txt

        if ! diff -u "$scannerList" reader-names.txt; then
          echo "" >&2
          echo "nix/use-package.nix regex scanner disagrees with the Emacs" >&2
          echo "reader on the use-package heads in lisp/ (< scanner, > reader)." >&2
          echo "A commented-out, quoted, or string-embedded '(use-package X'" >&2
          echo "is the usual cause. Reconcile the source or the scanner." >&2
          exit 1
        fi
        touch $out
      '';

  # legacyPackages.emacs-packages must cover every declared package with
  # a derivation. The expected set is recomputed from lisp/ +
  # nix-provided-packages.nix, so a broken resolver or dropped name
  # fails. Eval-only: never builds packages.
  emacs-packages-eval =
    let
      set = import ./emacs-package-set.nix { inherit pkgs; };
      up = import ./use-package.nix { inherit lib; };
      scanned = up.scanDirectoryWithDoc ../lisp;
      docEntries = lib.filter (e: !e.ensureNil) (lib.concatMap (s: s.entries) scanned);
      expected = lib.unique ((map (e: e.name) docEntries) ++ (import ./nix-provided-packages.nix)) ++ [
        "treesit-grammars"
      ];
      missing = lib.filter (n: !(set.byName ? ${n})) expected;
      nonDrv = lib.filter (n: !lib.isDerivation set.byName.${n}) (
        lib.filter (n: set.byName ? ${n}) expected
      );
    in
    pkgs.runCommandLocal "check-emacs-packages-eval"
      {
        inherit missing nonDrv;
        count = toString (
          builtins.length (lib.filter (n: n != "recurseForDerivations") (builtins.attrNames set.byName))
        );
      }
      ''
        test -z "$missing" || { echo "missing from legacyPackages.emacs-packages:"; echo "$missing"; exit 1; }
        test -z "$nonDrv" || { echo "not derivations:"; echo "$nonDrv"; exit 1; }
        # Floor against scanner rot (mirrors elisp-lint's file-count guard).
        test "$count" -ge 100 || { echo "only $count packages — the scan or resolver is broken"; exit 1; }
        touch $out
      '';

  # Elisp byte-compilation (warnings as errors). Same derivation as
  # module.nix' `compiledConfig' (see nix/config-compiled.nix); a
  # non-empty check output is fine.
  elisp-compile = import ./config-compiled.nix {
    inherit pkgs;
    emacs = elispEmacs;
  };

  # ERT suite. Sandbox-safe: no devenv binary, network or subprocesses.
  elisp-test =
    pkgs.runCommand "check-elisp-test"
      {
        nativeBuildInputs = [ elispEmacs ];
        src = elispTestSrc;
      }
      ''
        cd $src
        emacs --batch -L lisp -L test \
          --eval '(dolist (f (directory-files "test" t "\\.el$")) (load f nil t))' \
          -l ert -f ert-run-tests-batch-and-exit
        touch $out
      '';

  # Full-startup smoke test
  #
  # The only gate that evaluates every use-package :config block (e.g. a
  # stale ELPA package shadowing a built-in), against elisp-compile's
  # closure:
  #   • `emacs --batch' loads no init, so load early-init.el and init.el
  #     explicitly (no -q: site activation puts packages on load-path);
  #   • user-emacs-directory and HOME point at a writable copy, since
  #     the config writes under var/;
  #   • `use-package-always-ensure nil' after early-init.el, so no
  #     :ensure triggers `package-install' (no network in the sandbox).
  # use-package demotes a failing :config to an "Error (use-package)"
  # warning, so a clean exit is not enough: fail on that marker and on
  # the autoload-failure signature. Benign `Warning (emacs)' lines are
  # not matched.
  config-startup =
    pkgs.runCommand "check-config-startup"
      {
        nativeBuildInputs = [ elispEmacs ];
        src = elispSrc;
      }
      ''
        set -euo pipefail
        workdir="$(mktemp -d)"
        cp -rL "$src"/. "$workdir"/
        chmod -R u+w "$workdir"
        export HOME="$workdir"

        emacs --batch \
          --eval "(setq user-emacs-directory (file-name-as-directory \"$workdir\"))" \
          -l "$workdir/early-init.el" \
          --eval '(setq use-package-always-ensure nil)' \
          -l "$workdir/init.el" \
          --eval '(message "jotain: config startup complete")' \
          > "$workdir/startup.log" 2>&1

        cat "$workdir/startup.log"

        if grep -qE "Error \(use-package\)|failed to define function" "$workdir/startup.log"; then
          echo "FAIL: a use-package :config block errored during startup" >&2
          echo "(commonly a stale ELPA package shadowing an Emacs built-in)" >&2
          exit 1
        fi
        if ! grep -qF "jotain: config startup complete" "$workdir/startup.log"; then
          echo "FAIL: startup aborted before loading the whole config" >&2
          exit 1
        fi
        touch $out
      '';
}
