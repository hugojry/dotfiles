# xorg
alias xup="xrdb ~/.Xresources"

# vim
if type "nvim" 2>/dev/null; then
    alias vim="nvim"
fi 
alias svim="'vim' --servername VIM"

# ssh painlessness
alias add-identity='eval $(ssh-agent) && ssh-add'

#rbenv
alias rb='eval "$(rbenv init -)"'
