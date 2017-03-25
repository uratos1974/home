### -*- mode: shell-script; coding: utf-8-unix; -*-
###
### Startup file for Z shell
###
###

ZSHDIR=$HOME/.zsh.d

typeset -U path manpath
   path=($HOME/opt/*/bin(N-/) $HOME/bin(N-/) $path)
manpath=($HOME/opt/*/man(N-/) $HOME/man(N-/) $manpath)
export PATH MANPATH

export EDITOR=vim
export PAGER=less
export LESS=-MR
export LESSHISTFILE=-
export LS_COLORS='di=34:ln=32:so=31:pi=31:ex=33:bd=31:cd=31:su=1;33:sg=1;33:tw=1;34:ow=1;34'
export LSCOLORS=excxbxbxdxbxbxDxDxExEx

HISTFILE=$ZSHDIR/history
HISTSIZE=10000
SAVEHIST=$HISTSIZE
LISTMAX=0

setopt PUSHD_TO_HOME
setopt NO_LIST_BEEP
setopt LIST_PACKED
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt SHARE_HISTORY
setopt PROMPT_SUBST

bindkey -e

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable svn git
zstyle ':vcs_info:*' formats       '%F{green}%s%f:%F{green}%b%f%c%u | '
zstyle ':vcs_info:*' actionformats '%F{green}%s%f:%F{green}%b%f%c%u:%F{red}%a%f | '
zstyle ':vcs_info:svn:*' branchformat '%F{green}%b%f:%F{green}%r%f'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr   ':%F{magenta}%BS%b%f'
zstyle ':vcs_info:git:*' unstagedstr ':%F{magenta}%BU%b%f'
function precmd() { LANG=C vcs_info }
PROMPT=$'\n%B[%b $vcs_info_msg_0_%F{blue}%~%f %B]%b\n%F{yellow}%B%#%b%f '

autoload -U compinit
compinit -d $ZSHDIR/compdump
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

alias   ls="/bin/ls --color=auto -F"
alias   ll="ls -h -l"
alias   la="ls -a"
alias  lla="ll -a"
alias   pu="pushd"
alias   po="popd"
alias   ..="cd .."
alias  ...="cd ../.."
alias    g="git"
alias    l="less"
alias    v="vim"
alias -g L="| less"
alias -g G="| grep"

limit coredumpsize 0 &> /dev/null

SRC=$ZSHDIR/zshrc-o-${OSTYPE%%[^a-z]*}
[[ -r $SRC ]] && source $SRC

SRC=$ZSHDIR/zshrc-h-${HOST%%.*}
[[ -r $SRC ]] && source $SRC

### End of File
