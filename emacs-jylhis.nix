# emacs-jylhis.nix — Build the jylhis/emacs Meson branch.
#
# This is intentionally separate from emacs.nix.  The pinned jylhis/emacs
# branch has removed the upstream autotools build system and cannot use the
# cache-parity wrapper that emacs.nix builds around emacs-overlay's
# emacs-git.
{
  system ? builtins.currentSystem,
  pkgs ?
    import
      (
        let
          lock = builtins.fromJSON (builtins.readFile ./flake.lock);
          n = lock.nodes.nixpkgs.locked;
        in
        fetchTarball {
          url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz";
          sha256 = n.narHash;
        }
      )
      {
        inherit system;
        config.allowUnfree = true;
      },

  src ? null,
  rev ? "eaf289b4f7414744a23912ab7aae0a518d998242",
  hash ? "sha256-ZbwX8kKNWpV6BbaqEFE6ZB4oNuqwtdg6QTNcKX1qPBY=",

  # The Meson branch has GTK3 terminal/X11 wiring, but NS/PGTK are not wired in
  # the source tree yet.  Keep the default terminal-only so it works on Darwin
  # and Linux without changing the normal Jotain Emacs package.
  noGui ? true,
  withGTK3 ? false,
  withNativeCompilation ? false,
  withTreeSitter ? true,
  withSQLite3 ? true,
  withWebP ? true,
  withCairo ? !noGui && withGTK3,
  withDbus ? pkgs.stdenv.hostPlatform.isLinux,
  withSystemd ? pkgs.stdenv.hostPlatform.isLinux,
}:

let
  inherit (pkgs) lib stdenv;
  feature = enabled: if enabled then "enabled" else "disabled";
  sourceRev = src.rev or rev;
  shortRev = builtins.substring 0 7 sourceRev;
  source =
    if src != null then
      src
    else
      pkgs.fetchFromGitHub {
        owner = "jylhis";
        repo = "emacs";
        inherit rev hash;
      };
in
stdenv.mkDerivation (_finalAttrs: {
  pname = "jylhis-emacs";
  version = "31.0.50-${shortRev}";

  src = source;

  strictDeps = true;

  nativeBuildInputs = with pkgs; [
    meson
    ninja
    pkg-config
    python3
    texinfo
  ];

  buildInputs =
    with pkgs;
    [
      cairo
      fontconfig
      freetype
      giflib
      gmp
      gnutls
      harfbuzz
      jansson
      lcms2
      libjpeg
      libgcrypt
      libpng
      librsvg
      libtiff
      libwebp
      libxml2
      ncurses
      pango
      sqlite
      tree-sitter
      zlib
    ]
    ++ lib.optionals (withDbus && stdenv.hostPlatform.isLinux) [ dbus ]
    ++ lib.optionals (withSystemd && stdenv.hostPlatform.isLinux) [ systemdLibs ]
    ++ lib.optionals (!noGui && withGTK3) (
      [
        gtk3
      ]
      ++ (with xorg; [
        libICE
        libSM
        libX11
        libXcomposite
        libXext
        libXfixes
        libXft
        libXi
        libXinerama
        libXrandr
        libXrender
        libXt
        libXtst
      ])
    )
    ++ lib.optionals withNativeCompilation [ libgccjit ];

  mesonFlags = [
    "-Dtoolkit=${if !noGui && withGTK3 then "gtk3" else "none"}"
    "-Dpgtk=disabled"
    "-Dns=disabled"
    "-Dnative-compilation=${if withNativeCompilation then "yes" else "no"}"
    "-Dtree-sitter=${feature withTreeSitter}"
    "-Dsqlite3=${feature withSQLite3}"
    "-Dwebp=${feature withWebP}"
    "-Dcairo=${feature withCairo}"
    "-Dxft=disabled"
    "-Dxim=disabled"
    "-Dxpm=disabled"
    "-Dxinput2=disabled"
    "-Dxdbe=disabled"
    "-Dtoolkit-scroll-bars=disabled"
    "-Dlibotf=disabled"
    "-Dm17n-flt=disabled"
    "-Ddbus=${feature withDbus}"
    "-Dlibsystemd=${feature withSystemd}"
    "-Dgsettings=disabled"
    "-Dgpm=disabled"
    "-Dselinux=disabled"
    "-Dmailutils=disabled"
    "-Dcompress-install=true"
  ];

  postPatch = ''
    substituteInPlace lisp/loadup.el \
      --replace-warn '(emacs-repository-get-version)' '"${sourceRev}"' \
      --replace-warn '(emacs-repository-get-branch)' '"dev"'
  '';

  postInstall = ''
        charscript=$(find "$NIX_BUILD_TOP" -name charscript.el -print -quit)
        emoji_zwj=$(find "$NIX_BUILD_TOP" -name emoji-zwj.el -print -quit)
        install -Dm444 "$charscript" \
          $out/share/emacs/31.0.50/lisp/international/charscript.el
        install -Dm444 "$emoji_zwj" \
          $out/share/emacs/31.0.50/lisp/international/emoji-zwj.el

        cat > $out/share/emacs/31.0.50/lisp/dired-loaddefs.el <<'EOF'
    ;;; dired-loaddefs.el --- minimal generated autoloads for Meson install

    ;;; Code:

    (autoload 'dired-goto-subdir "dired-aux")

    ;;; dired-loaddefs.el ends here
    EOF
  '';

  meta = {
    description = "GNU Emacs from the jylhis/emacs Meson branch";
    homepage = "https://github.com/jylhis/emacs";
    license = lib.licenses.gpl3Plus;
    mainProgram = "emacs";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
})
