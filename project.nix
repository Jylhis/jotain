{
  meta = {
    description = "GNU Emacs 31 configuration with a Nix build layer";
    stack = "nix";
    status = "stable";
    license = "GPL-3.0-or-later";
    platforms = [
      "x86_64-linux"
    ];
  };

  # No package.nix: the distribution builds through nix/outputs.nix, which
  # flake.nix, default.nix and the root nix/inputs.nix all call.
  deploy = {
    type = "none";
  };

  # Read outside the project directory (project.nix(5) uses).
  uses = [
    "projects/libs/design"
  ];
}
