# Installation

Install dependencies:
```bash
sudo dnf install emacs clang clang-devel global stow cmake g++ clang-devel&&
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
- https://github.com/tuhdo/emacs-c-ide-demo
- [Backups](http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files)
- [Using generated database from GNU Global](https://tuhdo.github.io/c-ide.html#sec-2)
- enable global fic-mode
- [refactor](https://github.com/tuhdo/semantic-refactor)
- problem: when melpa down editor does not load
- tryto speed up startup
- check CEDET
[Read otsikko deft -> alaspäin](http://aaronbedra.com/emacs.d/)
- helm find function from project
- switch between .hpp -> .cpp
- find declaration
- find usages
- [gtags](http://spacemacs.org/layers/gtags/README.html)
- clang format 
- project file support (sublime-project, .pro, cmake etc.)
- .h, .hpp -> .c, .cpp, cxx
- find some alternative white theme (solarized-light)
### Magit ?? maybe
- git show changed lines
- git conflict solve

- find all todos etc. in git repo and show them in helm
- keybind decrease increase size of splitted windowA
- http://spacemacs.org/layers/+lang/c-c++/README.html
- http://spacemacs.org/layers/+lang/html/README.html
- [git](http://spacemacs.org/layers/+source-control/git/README.html)
- [hydra](http://oremacs.com/2015/01/20/introducing-hydra/)
