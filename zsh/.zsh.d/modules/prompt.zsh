# ripped shamelessly from the prezto damoekri theme
prompt_pwd() {
  local pwd="${PWD/#$HOME/~}"

  if [[ "$pwd" == (#m)[/~] ]]; then
    _prompt_pwd="$MATCH"
    unset MATCH
  else
    _prompt_pwd="${${${${(@j:/:M)${(@s:/:)pwd}##.#?}:h}%/}//\%/%%}/${${pwd:t}//\%/%%}"
  fi
}

prompt_rbenv() {
    unset _rbenv_info
    if [[ "$(which ruby)" == "$HOME/.rbenv"* ]]; then
        _rbenv_info=" r:$(rbenv version-name)"
    fi
}

prompt_pyenv() {
    unset _pyenv_info
    if [[ "$(which python)" == "$HOME/.pyenv"* ]]; then
        _pyenv_info=" p:$(pyenv version-name)"
    fi
}

prompt_precmd() {
    prompt_pwd
    prompt_git
    prompt_rbenv
    prompt_pyenv
}

prompt_git() {
    unset _git_info

    if [ -z "$(git rev-parse --is-inside-work-tree 2> /dev/null)" ]; then
        return 1
    fi

    _git_branch=$(git rev-parse --abbrev-ref HEAD)
    if [ -n "$(git status --porcelain)" ]; then
        _git_dirty="+"
    else
        _git_dirty=""
    fi
    
    _git_info=" ${_git_branch}${_git_dirty}"
}

prompt_setup() {
    setopt prompt_subst
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd prompt_precmd

    PROMPT='%F{cyan}${_prompt_pwd}%F{12}${_git_info}%f %F{cyan}»%f '
    RPROMPT='%F{red}${_rbenv_info}%f%F{green}${_pyenv_info}%f'
}

prompt_setup "$@"
