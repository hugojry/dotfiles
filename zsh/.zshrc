source $HOME/.zshrc-portable

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/bin/core_perl:$HOME/.local/bin:$HOME/.local/opt"

# theming resources
export COLORS=~/.dotfiles/colors
export WALLS=~/.dotfiles/wallpapers

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# thefuck
#eval $(thefuck --alias)

# cabal
export PATH="$PATH:$HOME/.cabal/bin"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PATH:$PYENV_ROOT/bin"
#SLOOWWWWW
alias pyinit='eval "$(pyenv init -)"'

export PATH="$PATH:$HOME/.composer/vendor/bin"
