---
title: JOTAIN
section: 7
header: Jotain Manual
footer: jotain 2026.07
date: 2026-07-21
---

# NAME

jotain — a custom GNU Emacs configuration built from scratch

# SYNOPSIS

**just** *run* | *debug* | *tty*

**just** *build* · **just** *check* · **just** *compile* · **just** *fmt* · **just** *update*

# DESCRIPTION

*Jotain* is Finnish for "something" — a GNU Emacs 30+ configuration with
no framework underneath. Instead of layering on Doom or Spacemacs, it is
built from scratch: Nix builds the editor from source, plain Elisp
configures it.

The repository ships both a modular Elisp configuration
(*early-init.el*, *init.el*, *lisp/init-\*.el*) and the Nix expressions
that build Emacs itself (*emacs.nix*, *overlay.nix*, *default.nix*,
*flake.nix*). The two are coupled: the dev shell's *emacs* is exactly
what **just build** produces.

# MANUAL SECTIONS

The full manual is generated from *docs/* and available as HTML, Info,
and this man page. Chapters:

*introduction(7)*
:   what jotain is, and why

*installation(7)*
:   nix, home-manager, devenv

*quickstart(7)*
:   running in minutes

*architecture(7)*
:   overview · nix-build · modules

*configuration(7)*
:   init · early-init · packages

*usage(7)*
:   launching · devenv · ai-screenshot

*keybindings(7)*
:   the full chord map

*ergonomics(7)*
:   keyboard, posture, flow

# FILES

*/docs/*
:   full documentation index (HTML)

*/manual/*
:   the Jotain manual, one page per chapter

*/manual/jotain.info*
:   the same manual as an Info file — install and read with
    **C-h i d m Jotain RET**

*/man/*
:   this page and the Emacs man pages

*/info/emacs/*, */info/elisp/*
:   the GNU Emacs manual and the Emacs Lisp reference manual, rendered
    from the exact Emacs source revision Jotain builds

*/options/*
:   Nix module options reference (Home Manager, NixOS/nix-darwin, devenv)

*/help/packages/*
:   every Emacs package Jotain ships, with the reason it is included

# SEE ALSO

**emacs**(1), **emacsclient**(1), **nix**(1), **just**(1)

<https://github.com/Jylhis/jotain>, <https://jylhis.com>
