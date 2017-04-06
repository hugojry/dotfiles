# ripped shamelessly from the prezto damoekri theme
autoload -U zgitinit
zgitinit

prompt_pwd() {
    _prompt_pwd="%1~"
}

prompt_rbenv() {
    unset _rbenv_info
    if [[ ! -z $RBENV_SHELL ]]; then
        _rbenv_info=" r:$(rbenv version-name)"
    fi
}

prompt_pyenv() {
    unset _pyenv_info
    if [[ ! -z $PYENV_SHELL ]]; then
        _pyenv_info=" p:$(pyenv version-name)"
    fi
}

prompt_setup() {
    setopt prompt_subst
    precmd_functions+='prompt_pwd'
    precmd_functions+='prompt_rbenv'
    precmd_functions+='prompt_pyenv'

    local verbose
    if [[ $TERM == screen* ]] && [ -n "$STY" ]; then
        verbose=
    else
        verbose=1
    fi

    PROMPT='%F{cyan}${_prompt_pwd}$(prompt_wunjo_scm_branch)${_git_info}%f %F{cyan}$%f '
    RPROMPT='%F{red}${_rbenv_info}%f%F{green}${_pyenv_info}%f'

    export PROMPT RPROMPT
}

prompt_wunjo_scm_branch() {
    zgit_isgit || return

    echo -n " %F{blue}$(zgit_head)"

    if zgit_inworktree; then
        if ! zgit_isindexclean; then
            echo -n "%F{green}+"
        fi

        local -a dirty
        if ! zgit_isworktreeclean; then
            dirty+='!'
        fi

        if zgit_hasunmerged; then
            dirty+='*'
        fi

        if zgit_hasuntracked; then
            dirty+='?'
        fi

        if [ $#dirty -gt 0 ]; then
            echo -n "%F{red}${(j::)dirty}"
        fi
    fi
}

prompt_setup "$@"

# vim:set ft=zsh:
