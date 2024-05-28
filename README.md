# Install dependencies

<!--toc:start-->
- [Install dependencies](#install-dependencies)
<!--toc:end-->

Install tree-sitter grammars:
`M-x tree-sitter-langs-install-grammars`

```shell
go install golang.org/x/tools/gopls@latest
go install github.com/rogpeppe/godef@latest
go install github.com/go-delve/delve/cmd/dlv@latest
curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash
apt install clangd

# Nix profile
nix profile install github:nixos/nixpkgs#nixd nixpkgs#gopls nixpkgs#godef nixpkgs#delve \
nixpkgs#source-code-pro nixpkgs#clang nixpkgs#asm-lsp


# Haskell
ghcup install hls

haskell-language-server-

# ansible
npm i -g @ansible/ansible-language-server


pip install "debugpy"

# rust
rustup component add rust-analyzer


```

In emacs remember to run
```
M-x all-the-icons-install-fonts
```

# Daemon
`~/.config/systemd/user/emacs.service`
```
[Unit]
Description=Emacs Daemon

[Service]
Type=notify
Type=notify
ExecStart=/snap/emacs/current/usr/bin/emacs --fg-daemon
ExecStop=/snap/emacs/current/usr/bin/emacsclient --eval "(kill-emacs)"
Restart=on-failure

[Install]
WantedBy=default.target
```

Enable server
```
systemctl --user enable emacs.service
```

Launch emacsclient GUI
```
emacsclient -c
```

launch emacsclient terminal
```
emacsclient -t
```
