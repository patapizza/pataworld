#!/bin/bash

echo "Copying ~/.vimrc, ~/.zshrc and ~/.vim..."
[[ -f ~/.vimrc ]] && mv ~/.vimrc ~/.vimrc.backup
[[ -f ~/.zshrc ]] && mv ~/.zshrc ~/.zshrc.backup
[[ -d ~/.vim ]] && mv ~/.vim ~/.vim.backup

mv .vimrc ~/.vimrc
mv .zshrc ~/.zshrc

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
echo "[vim] Installing clojure plugins..."
git clone https://github.com/tpope/vim-fireplace.git
git clone https://github.com/guns/vim-clojure-static.git
git clone https://github.com/guns/vim-clojure-highlight.git
git clone https://github.com/vim-scripts/paredit.vim.git
git clone https://github.com/tpope/vim-surround.git

echo "done!"
