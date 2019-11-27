#!/usr/bin/env zsh

# Complétion
autoload -U compinit
compinit
# et completion de bash
autoload -U bashcompinit
bashcompinit

source .zsh_secrets

# word movement
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

#Insensible à la casse
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

#compinstall
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                             /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
# Crée un cache des complétion possibles
# très utile pour les complétion qui demandent beaucoup de temps
# comme la recherche d'un paquet aptitude install moz<tab>


zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh_cache
# des couleurs pour la complétion
# faites un kill -9 <tab><tab> pour voir :)
zmodload zsh/complist
setopt extendedglob
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=36=31"

# Correction des commandes
setopt correctall

autoload colors; colors

#Charge liquidprompt
source ~/build/liquidprompt/liquidprompt

# Les alias marchent comme sous bash
alias ls='ls --color=auto'
alias ll='ls --color=auto -lh'
alias lll='ls --color=auto -lh | less'
alias la='ls --color=auto -lha'

# common typos and/or shortcuts
alias xs='cd'
alias sl='ls'
alias gtkcc='gcc `pkg-config --cflags --libs gtk+-3.0`'
alias ec='emacs -nw'

# Alias d'i3
external=DP-1
laptop=eDP-1

alias exleftlapright='xrandr --output $external --auto --left-of $laptop'
alias exrightlapleft='xrandr --output $laptop --auto --left-of $external'
alias externalonly='xrandr --output $laptop --off && xrandr --output $external --auto'
alias laptoponly='xrandr --output $external --off && xrandr --output $laptop --auto'
alias duplicatescreens='xrandr --output $external --same-as $laptop'

# Alias df -h
alias df='df -h'

# Un grep avec des couleurs :
export GREP_COLOR=31
alias grep='grep --color=auto'

# My text editor
export EDITOR=/usr/bin/emacs

alias d="wget -c --user=$download_user --password=$download_password"

#Activer l'historique des commandes:
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
export HISTFILE SAVEHIST

zstyle ':completion:*' verbose true

export PATH="$PATH:$(ruby -e 'print Gem.user_dir')/bin"
export PATH=$PATH:~/bin

export PATH=$PATH:~/.cargo/bin

unsetopt correct_all

# HOL config
export HOLDIR=$HOME/build/HOL
export PATH=$HOLDIR/bin:$PATH
export CAKEMLDIR=$HOME/build/cakeml

# LEM config
export PATH=${PATH}:${HOME}/build/lem
export LEMLIB=${HOME}/build/lem/library

# Snap config
export PATH=${PATH}:/snap/bin

alias pwdfind="grep -rnw '.' -e"

# opam configuration
test -r /home/qladevez/.opam/opam-init/init.zsh && . /home/qladevez/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
