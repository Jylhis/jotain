{
  meta = {
    description = "Jotain — GNU Emacs 31 configuration with a Nix build layer";
    stack = "nix";
    status = "stable";
    license = "GPL-3.0-or-later";
    platforms = [
      "x86_64-linux"
      "x86_64-darwin"
    ];
  };

  # No package.nix: the Emacs distribution builds through nix/inputs.nix
  # (the computed jotain attrset) and the flakeless shim default.nix.
  deploy = { type = "none"; };
}
