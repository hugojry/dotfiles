# ripped shamelessly from the prezto damoekri theme
autoload -U zgitinit
zgitinit

prompt_pwd() {
    _prompt_pwd="%1~"
}

prompt_setup() {
    setopt prompt_subst
    precmd_functions+='prompt_pwd'

    local verbose
    if [[ $TERM == screen* ]] && [ -n "$STY" ]; then
        verbose=
    else
        verbose=1
    fi

    export PROMPT='%F{cyan}${_prompt_pwd}$(prompt_wunjo_scm_branch)${_git_info}%f %F{cyan}$%f '
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
