---
title: EMACS-CONFIG
section: 7
header: Emacs Configuration Manual
footer: emacs-config 2026.09
date: 2026-09-29
---

# NAME

emacs-config — a custom GNU Emacs configuration built from scratch

# SYNOPSIS

**just** *run-built* | *run-built-debug*

**just** *build* · **just** *check* · **just** *fmt* · **just** *update*

# DESCRIPTION

A GNU Emacs 31 configuration
(floor: Emacs 30.1) built from scratch, with no framework such as Doom
or Spacemacs underneath. Nix builds the editor; plain Elisp configures
it.

The repository ships a modular Elisp configuration (*early-init.el*,
*init.el*, *lisp/init-\*.el*) and the Nix expressions that build Emacs
(*emacs.nix*, *overlay.nix*, *default.nix*, *flake.nix*). The dev shell
provides tooling only, not Emacs; **just run-built** builds the editor
via Nix and launches it with this configuration.

# MANUAL SECTIONS

The full manual is generated from *docs/* and available as HTML, Info,
and this man page. Chapters:

*introduction(7)*
:   what the configuration is, and why

*installation(7)*
:   nix, home-manager, devenv

*quickstart(7)*
:   running in minutes

*architecture(7)*
:   overview · nix-build · modules

*configuration(7)*
:   init · early-init · packages

*usage(7)*
:   launching · devenv · notebooks · ai-screenshot

*keybindings(7)*
:   the full chord map

*ergonomics(7)*
:   keyboard, posture, flow

# FILES

*/docs/*
:   full documentation index (HTML)

*/manual/*
:   the manual, one page per chapter

*/manual/emacs-config.info*
:   the same manual as an Info file — install and read with
    **C-h i d m Emacs config RET**

*/man/*
:   this page and the Emacs man pages

*/info/emacs/*, */info/elisp/*
:   the GNU Emacs manual and the Emacs Lisp reference manual, rendered
    from the exact Emacs source revision the configuration builds

*/options/*
:   Nix module options reference (Home Manager, NixOS, devenv)

*/help/packages/*
:   every Emacs package the configuration ships, with the reason it is included

# SEE ALSO

**emacs**(1), **emacsclient**(1), **nix**(1), **just**(1)

<https://github.com/Jylhis/jotain>, <https://jylhis.com>
