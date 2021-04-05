PROMPT='
%{$fg[yellow]%}%~%{$reset_color%} $(git_prompt_info)
%(?,%{$fg[green]%};%{$reset_color%},%{$fg[red]%}!%{$reset_color%}) '

ZSH_THEME_GIT_PROMPT_PREFIX="| %{$fg_bold[blue]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} ✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%} ✔%{$reset_color%}"

function preexec() {
  timer=${timer:-$SECONDS}
}

function precmd() {
  if [ $timer ]; then
    timer_elapsed=$(($SECONDS - $timer))
    RPROMPT='%{$fg[blue]%}${timer_elapsed}s%f'
    unset timer
  fi
}

autoload -Uz add-zsh-hook
add-zsh-hook preexec preexec
add-zsh-hook precmd precmd
