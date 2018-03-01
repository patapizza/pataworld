export HISTSIZE=100000 
export SAVEHIST=100000
export HISTFILE=~/.history
setopt share_history
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE

bindkey "^R" history-incremental-search-backward
bindkey "^[[A" up-line-or-history
bindkey "^[[B" down-line-or-history

bindkey -v
export KEYTIMEOUT=1

export TERM=xterm-256color

typeset -Ag FX FG BG
FX=(
reset     "%{[00m%}"
bold      "%{[01m%}" no-bold      "%{[22m%}"
italic    "%{[03m%}" no-italic    "%{[23m%}"
underline "%{[04m%}" no-underline "%{[24m%}"
blink     "%{[05m%}" no-blink     "%{[25m%}"
reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)
for color in {000..255}; do
  FG[$color]="%{[38;5;${color}m%}"
  BG[$color]="%{[48;5;${color}m%}"
done
ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}
function spectrum_ls() {
  for code in {000..255}; do
    print -P -- "$code: %F{$code}$ZSH_SPECTRUM_TEXT%f"
  done
}

export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

export PS1="%F{244}%T%f %(!.%F{202}.%F{40})%n%f@%F{37}%m%f %F{185}%~%f $ "

export EDITOR='vim'

alias ll='ls -l'
alias soxx='sox -d -r 16k -b 16 -c 1 -e signed-integer'

# git
alias g='git status'
alias ga='git add'
alias gc='git commit -m'
alias gp='git push'
alias gl='git pull'
alias gb='git branch'
alias gck='git checkout'
alias gd='git diff'
