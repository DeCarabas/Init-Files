# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=2000
setopt autocd
setopt histappend
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/doty/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

PS1='%n@%m:%~$ '

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

if [ -f ~/.zsh_aliases ]; then
    . ~/.zsh_aliases
fi

[ -s "/Users/doty/.dnx/dnvm/dnvm.sh" ] && . "/Users/doty/.dnx/dnvm/dnvm.sh" # Load dnvm
