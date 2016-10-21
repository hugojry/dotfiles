rbenv_prompt() {
    if [[ "$(which ruby)" == "$HOME/.rbenv"* ]]; then
        rbenv_status="rbenv:$(rbenv version-name)"
    else
        rbenv_status=""
    fi

    RPROMPT="%F{1}$rbenv_status%f"
}

rbenv_prompt_setup() {
    autoload -Uz add-zsh-hook

    add-zsh-hook precmd rbenv_prompt
}

rbenv_prompt_setup "$@"
