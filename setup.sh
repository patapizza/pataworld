#!/bin/bash

[[ ! -x ./setup.sh ]] && echo "Couldn't find executable ./setup.sh" && exit 1

echo "Copying .vimrc and .zshrc..."
[[ -f ~/.vimrc ]] && mv ~/.vimrc ~/.vimrc.backup
[[ -f ~/.zshrc ]] && mv ~/.zshrc ~/.zshrc.backup
cp .vimrc ~/.vimrc
cp .zshrc ~/.zshrc
rm -rf .

# vim
## pathogen
echo "[vim] Installing pathogen..."
mkdir -p ~/.vim/autoload ~/.vim/bundle && \
  curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

cd ~/.vim/bundle

## solarized
echo "[vim] Installing solarized..."
git clone https://github.com/altercation/vim-colors-solarized.git

## clojure
echo "[vim] Installing clojure..."
git clone https://github.com/tpope/vim-fireplace.git
git clone https://github.com/guns/vim-clojure-static.git
git clone https://github.com/guns/vim-clojure-highlight.git

echo "done!"
