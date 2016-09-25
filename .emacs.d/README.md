# Installation
Install dependencies:
```bash
sudo dnf install emacs clang clang-devel global stow cmake gcc-c++ clang-devel&&
sudo python3 -m pip install Pygments jedi pylint
```

Clone configs
```bash
git clone https://github.com/Jylhis/dotfiles ~/dotfiles
cd ~/dotfiles
stow emacs
```

In emacs: M-x
`irony-install-server`
`jedi:install-server`

# Bytecompile config directory
`M-x byte-recompile-directory`

# Windows
Copy dotfiles to %appdata%/.emacs.d

# TODO
- remove line numbers from org-mode
- fix irony-c-header thing
- [emacs doom](https://www.reddit.com/r/emacs/comments/51jvai/making_modern_emacs_themes/)
- [CEDET](http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html)
- [alexott config](https://github.com/alexott/emacs-configs)
- [Using emacs as ide](http://stackoverflow.com/questions/63421/using-emacs-as-an-ide?noredirect=1&lq=1)
- [Learn emacs windows and frames](https://blasphemousbits.wordpress.com/2007/05/04/learning-emacs-part-4-buffers-windows-and-frames/)
- https://github.com/tuhdo/emacs-c-ide-demo
- [Using generated database from GNU Global](https://tuhdo.github.io/c-ide.html#sec-2)
- [refactor](https://github.com/tuhdo/semantic-refactor)
- [Read otsikko deft -> alaspäin](http://aaronbedra.com/emacs.d/)
- helm find function from project
- switch between .hpp -> .cpp
- find declaration
- find usages
- clang format 
- project file support (sublime-project, .pro, cmake etc.)
- .h, .hpp -> .c, .cpp, cxx
- git show changed lines
- git conflict solve
- find all todos etc. in git repo and show them in helm
- keybind decrease increase size of splitted windowA
- http://spacemacs.org/layers/+lang/c-c++/README.html
- http://spacemacs.org/layers/+lang/html/README.html
- [git](http://spacemacs.org/layers/+source-control/git/README.html)
- [hydra](http://oremacs.com/2015/01/20/introducing-hydra/)
