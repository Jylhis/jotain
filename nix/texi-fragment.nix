# nix/texi-fragment.nix — post-processing for a pandoc-generated Texinfo
# fragment that docs/jotain.texi `@include's.
#
# Pandoc's texinfo writer emits a standalone document (@node/@top/@menu
# plus @refs between those nodes). In the master manual the nodes would
# collide with docs/jotain.texi's and the @refs would dangle; makeinfo
# re-derives nodes from the surviving @section hierarchy.
#
# Used by nix/info-manual.nix, nix/options-doc.nix, nix/packages-doc.nix
# and nix/emacs-api-doc.nix.
{
  # awk program: drop the @menu block and every @node / @top line.
  stripScaffolding = ''
    /^@menu$/     { in_menu = 1; next }
    /^@end menu$/ { in_menu = 0; next }
    in_menu       { next }
    /^@node /     { next }
    /^@top /      { next }
    { print }
  '';

  # sed -E script: flatten @ref{name,,text} and @ref{name} to plain text.
  flattenRefs = ''s/@ref\{[^,}]*,,([^}]*)\}/\1/g; s/@ref\{([^}]*)\}/\1/g'';
}
