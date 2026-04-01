linux:
  nix-build --argstr system x86_64-linux default.nix

run:
  nix-shell -p emacs --command 'emacs --debug-init --init-directory={{justfile_directory()}}'

clean:
  rm -r eln-cache result