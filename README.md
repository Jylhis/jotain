# A reasonable Emacs config

This started as fork of [Purcell's emacs config](https://github.com/purcell/emacs.d)
that I have continued to tweak since 2017. I have removed much of the
OS X specific stuff and some useless features/languages. This config is mainly
used on Linux and more mostly on Fedora and openSUSE.

This config support atleast these languages
* C
* C++
* CSS
* Common Lisp (with Slime)
* Erlang
* GO
* HTML
* Javascript
* Markdown
* PHP
* Python


In particular, there's a nice config for *autocompletion* with
[company](https://company-mode.github.io/), and
[flycheck](http://www.flycheck.org) is used to immediately highlight
syntax errors in number of languages.

## Other requirements

To make the most of the programming language-specific support in this
config, further programs will likely be required, particularly those
that [flycheck](https://github.com/flycheck/flycheck) uses to provide
on-the-fly syntax checking.

#### Font
Name: inconsolata
Size: 13

## Installation

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the
`init.el` contained in this repo ends up at `~/.emacs.d/init.el`:

```
git clone https://gitlab.com/Jylhis/Emacs ~/.emacs.d
```

Upon starting up Emacs for the first time, further third-party
packages will be automatically downloaded and installed. If you
encounter any errors at that stage, try restarting Emacs, and possibly
running `M-x package-refresh-contents` before doing so.

## Updates

Update the config with `git pull`. You'll probably also want/need to update
the third-party packages regularly too:

<kbd>M-x package-list-packages</kbd>, then <kbd>U</kbd> followed by <kbd>x</kbd>.

You should usually restart Emacs after pulling changes or updating
packages so that they can take effect. Emacs should usually restore
your working buffers when you restart due to this configuration's use
of the `desktop` and `session` packages.
