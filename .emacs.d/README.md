# Installation

Install dependencies:
```bash
sudo dnf install emacs clang clang-devel global stow \
zlib-devel openssl-devel lua-devel pkgconfig  &&
pip install Pygments
```

Compile rtags
```bash
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
mkdir build
cd build
cmake ..
make -j<number of cores>
sudo make install

# Optional: clean files
cd ../.. && rm -rf rtags
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
- enable global fic-mode
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
