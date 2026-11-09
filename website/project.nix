{
  meta = {
    description = "Jotain Emacs configuration site (landing page, manual, references) at docs.example.org";
    stack = "nix";
    status = "experimental";
    platforms = [ "x86_64-linux" ];
  };

  package = ./package.nix;

  tests = {
    # The vendored public/ds equals the in-tree design system's assets.
    ds-in-sync.hermetic = "sandbox";
  };

  # PUSH, not service: the generated pages (docs, manual, man, info, options,
  # API reference) need Nix, Cachix and the pinned Emacs, none of which
  # Cloudflare Workers Builds has (jylhis-deployment-model(7)). On green main the
  # dedicated workflow (.github/workflows/emacs-docs.yml) builds the full site
  # with Nix and runs `wrangler deploy`; `just deploy` is the manual path. That
  # workflow's existence also makes the generic sites.yml skip this project.
  deploy = {
    type = "static-site";
    mode = "push";
  };

  # docs.example.org -> a Workers custom domain (platform/engines/infra/emitters/
  # workers.nix binds the hostname to this project's Worker script, emacs-docs).
  requires.dns = "docs.example.org";

  # Read outside the project directory (project.nix(5) uses).
  uses = [
    "lib/design"
    "etc/emacs"
  ];
}
