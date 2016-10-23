rbenv_prompt() {
    if [[ "$(which ruby)" == "$HOME/.rbenv"* ]]; then
        rbenv_status="%F{1}rbenv:$(rbenv version-name)%f"
    else
        rbenv_status=""
    fi

    RPROMPT="$rbenv_status"
}

rbenv_prompt_setup() {
    autoload -Uz add-zsh-hook

    add-zsh-hook precmd rbenv_prompt
}

rbenv_prompt_setup "$@"
