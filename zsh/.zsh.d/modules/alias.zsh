# git
alias gs="git status"
alias gl="git log"
alias glo="git log --oneline"

# xorg
alias xup="xrdb ~/.Xresources"

# vim
if type "nvim" >/dev/null; then
    alias vim="nvim"
fi 
alias svim="'vim' --servername VIM"

# ssh painlessness
alias add-identity='eval $(ssh-agent) && ssh-add'

#pyenv
alias py='eval "$(pyenv init -)"'

#rbenv
alias rb='eval "$(rbenv init -)"'
