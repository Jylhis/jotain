# Installation

Install dependencies:
```bash
sudo dnf install emacs clang clang-devel global stow &&
pip install Pygments
```

Clone configs
```bash
git clone https://github.com/Jylhis/dotfiles ~/dotfiles
cd ~/dotfiles
stow emacs
```

In emacs: 
`irony-install-server`

# TODO
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

### Magit ?? maybe
- git show changed lines
- git conflict solve

- fic-mode: make hook thing to automatically enable it. todolist (TODO:, FIXME:, NOTE:)
- find all todos etc. in git repo and show them in helm
- keybind decrease increase size of splitted windowA
- http://spacemacs.org/layers/+lang/c-c++/README.html
- http://spacemacs.org/layers/+lang/html/README.html
- [git](http://spacemacs.org/layers/+source-control/git/README.html)
- [hydra](http://oremacs.com/2015/01/20/introducing-hydra/)
