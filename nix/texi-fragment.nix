# nix/texi-fragment.nix — post-processing for a pandoc-generated Texinfo
# fragment that docs/jotain.texi `@include's.
#
# Pandoc's texinfo writer emits a standalone document: a full
# @node/@top/@menu structure plus @ref cross-references between those
# nodes. A fragment pulled into the master manual must carry none of it —
# the nodes would collide with the node layout in docs/jotain.texi, and
# the @refs would point at targets that no longer exist. makeinfo
# re-derives nodes from the surviving @section hierarchy.
#
# Shared so a pandoc change needs one fix, not four. Consumed by
# nix/info-manual.nix, nix/options-doc.nix, nix/packages-doc.nix and
# nix/emacs-api-doc.nix — every fragment that reaches the Info manual.
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
