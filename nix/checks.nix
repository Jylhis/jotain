# nix/checks.nix — Flake check derivations for Jotain.
#
# Application and configuration checks live here (Nix linting, Elisp
# validation).  Dev-environment assertions live in devenv.nix enterTest.
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
  # `src' is the flake source (string-like) and lib.fileset needs a real
  # path, so the narrowed sources are built from ../. — the same tree,
  # the same idiom nix/info-manual.nix uses and the same constraint
  # flake.nix documents on `packages.site'.  Inside a flake, ../. is the
  # store copy of the source, so .git and .gitignore'd files (stray
  # *.elc, result symlinks) are already excluded.
  #
  # `src' itself stays a parameter: options-doc.nix computes
  # `srcPrefix = toString src + "/"' to rewrite declaration paths into
  # GitHub URLs, and handing it a fileset source would break that strip
  # and turn every `declarations' link into an absolute store path.
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
  nixSrc = fileset.toSource {
    root = repoRoot;
    fileset = fileset.union (fileset.fileFilter (f: lib.hasSuffix ".nix" f.name) repoRoot) (
      repoRoot + "/statix.toml"
    );
  };

  # Toolchain for the Elisp checks: the *inner* emacsWithPackages result
  # (nix/mk-overlay.nix `passthru.core'), not the outer jotain-emacs-full
  # wrapper. The wrapper only adds a runtime PATH, INFOPATH (→ jotainInfo)
  # and ASPELL_CONF, none of which a batch byte-compile or the ERT suite
  # reads; depending on it would instead pull in jotainInfo → packages-doc
  # + options-doc, invalidating these checks on unrelated docs/@doc edits.
  # `core' is also invariant under ordinary Elisp edits:
  # emacsWithPackagesFromUsePackage scans lisp/ only for package *names*,
  # so its store path moves only when the package set does.
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

  # Minimal stand-in for the nix-on-droid module system so the Jotain
  # nix-on-droid module evaluates here on x86_64 (eval only — the actual
  # aarch64 activation package is built on-device, not in CI).
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

  # Generated docstring-level API reference (etc/elisp-doc). A build check
  # only — the output is not checked into git, so there is no in-sync
  # gate. Heavy: realizes the config package closure and runs a batch
  # Emacs, so it lives with the deploy-only checks rather than PR CI's
  # lightweight subset (PR CI still exercises it transitively via `site`).
  emacs-api-doc = import ./emacs-api-doc.nix { inherit pkgs src; };

  # Checked-in Mintlify .mdx must match the generator
  #
  # docs/configuration/package-reference.mdx is checked in so the
  # Mintlify site can serve it without running Nix. The generator in
  # nix/packages-doc.nix is the source of truth, so we verify the
  # tracked file is byte-identical to the freshly generated one.
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

  # eca and gptel must offer the same OpenRouter model catalogue
  #
  # config/eca/config.json (providers.openrouter.models) and the gptel
  # OpenRouter backend in lisp/init-ai.el (:models) are two hand-kept
  # copies of one list, tied together only by cross-reference comments.
  # This makes the drift fatal. The elisp list is read with Emacs' own
  # reader (so a reformat can't fool a regex) and anchored on the
  # OpenRouter form, not the Ollama :models further down the file.
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

  # Vendored design system must match the pinned upstream rev
  #
  # website/public/ds is committed so website/public stays a no-build
  # shell, which is exactly how it drifted a whole major version behind
  # the Emacs themes built from the same repo. Both now read
  # nix/design-pin.nix, and this check makes the drift fatal.
  ds-in-sync =
    let
      dsAssets = import ./ds-assets.nix { inherit pkgs; };
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

  # flake.lock and devenv.lock must pin the same shared revs
  #
  # nix flake check never reads devenv.lock, so without this check the
  # only gate on lock drift is PR CI's `just verify` step — and nothing
  # at all guards pushes to main/next. Dependabot's "nix" ecosystem
  # bumps flake.lock alone, which is exactly how the two drift apart.
  # Same script the Justfile recipe runs, so the two can't disagree.
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

  # nix-on-droid module evaluation
  # Eval-only: the terminal Jotain Emacs the module selects builds on
  # aarch64 (on-device), but here we only assert the module evaluates
  # and wires EDITOR through environment.sessionVariables.
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

  # Build-side equivalent of the Emacs-provenance assertions that used
  # to live in devenv.nix `enterTest`. Emacs is no longer installed in
  # the dev shell, so we verify here that the jotainEmacs derivation
  # ships the expected binaries and that they run cleanly without
  # touching anything outside the store.
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
  # `runCommand', not `runCommandLocal', for the three Elisp checks:
  # runCommandLocal sets allowSubstitutes = false, which only wins when
  # rebuilding is cheaper than fetching, untrue once the rebuild needs a
  # multi-hundred-MB Emacs closure. Substituting the cached marker
  # (lint/test) or compiled-config tree (compile) from jylhis.cachix.org
  # is cheaper, and deploy.yml pushes them on main, so a PR touching
  # neither lisp/ nor test/ never pulls Emacs. The cheap checks below stay
  # runCommandLocal. (Dropping preferLocalBuild lets these run on remote
  # builders; harmless: none reads /proc, HOME, PATH or the network.)
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
            ;; The source is a narrowed lib.fileset (see the top of this
            ;; file).  A fileset that silently loses lisp/ or test/ would
            ;; leave this check passing over nothing, so make an
            ;; implausibly short file list fatal rather than green.
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
  # nix/use-package.nix finds `(use-package NAME' by regex over file
  # text, so a commented-out, quoted, or string-embedded occurrence would
  # be miscounted as a real package (or a genuine form missed). This
  # reads lisp/*.el with Emacs' own reader, collects the use-package
  # heads that appear as actual code, and diffs them against the
  # scanner's output. A divergence means the regex is over- or
  # under-matching. Implements deferred review Finding 48(c).
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

  # Elisp byte-compilation (warnings as errors)
  #
  # Shared with module.nix' `compiledConfig': on the default HM config
  # this is literally the same store path, so `home-manager switch'
  # substitutes CI's artifact instead of re-running Emacs.  `nix flake
  # check' only requires each check to build; a non-empty output is fine.
  elisp-compile = import ./config-compiled.nix {
    inherit pkgs;
    emacs = elispEmacs;
  };

  # Per-file twin of elisp-compile (nix/config-compiled-split.nix): one
  # derivation per source file, warnings as errors.  Gates only that the
  # split stays warning-clean in *clean per-file sessions* — which the
  # monolith's single shared session can mask (a feature loaded by an
  # earlier file hides a missing declare-function/defvar stub in a later
  # one).  Byte-for-byte comparison against the monolith is deliberately
  # NOT a check — see nix/elc-parity.nix (run via `just elc-parity`).
  elisp-compile-split = import ./config-compiled-split.nix {
    inherit pkgs;
    emacs = elispEmacs;
  };

  # Elisp unit tests (ERT, batch)
  # Pure-function tests only: no devenv binary, no network, no
  # subprocesses — safe inside the Nix sandbox.
  elisp-test =
    pkgs.runCommand "check-elisp-test"
      {
        nativeBuildInputs = [ elispEmacs ];
        src = elispSrc;
      }
      ''
        cd $src
        emacs --batch -L lisp -L test \
          --eval '(dolist (f (directory-files "test" t "\\.el$")) (load f nil t))' \
          -l ert -f ert-run-tests-batch-and-exit
        touch $out
      '';
}
