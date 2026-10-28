{
  meta = {
    description = "Jotain Emacs configuration site (landing page, manual, references)";
    stack = "nix";
    status = "experimental";
    platforms = [ "x86_64-linux" ];
  };

  package = ./package.nix;

  checks = {
    # The vendored public/ds equals the in-tree design system's assets.
    ds-in-sync.hermetic = "sandbox";
    # The generated site without the heavy /help/api/ reference.
    build.hermetic = "sandbox";
  };

  # Workers Builds deploys the committed public/ shell on green main. The
  # generated pages need Nix, which Workers Builds does not have; see jotain-site(8).
  deploy = {
    type = "static-site";
    mode = "service";
    preview = true;
    service.provider = "cloudflare-workers-builds";
  };
}
