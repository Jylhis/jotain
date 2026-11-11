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
    # The engine does not manage the Worker or docs.example.org: CI deploys it.
    provision = false;
  };

  # docs.example.org: the site's hostname. With deploy.provision = false the
  # engine binds no custom domain; set it to true to have workers.nix bind it.
  requires.dns = "docs.example.org";

  # Read outside the project directory (project.nix(5) uses).
  uses = [
    "projects/libs/design"
    "projects/distros/emacs"
  ];
}
