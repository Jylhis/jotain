{
  description = "GNU Emacs 31 configuration with a Nix build layer";

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://jylhis.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "jylhis.cachix.org-1:SIAw5iWjXRhLAmejqPy0PGuqH6bjCHIFVF9CiHmHRpE="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jylhis-design = {
      url = "github:Jylhis/design";
      flake = false;
    };
  };

  outputs =
    inputs:
    import ./nix/outputs.nix (
      {
        inherit (inputs) nixpkgs emacs-overlay treefmt-nix;
      }
      // import ./nix/world.nix { design = inputs.jylhis-design; }
    );
}
