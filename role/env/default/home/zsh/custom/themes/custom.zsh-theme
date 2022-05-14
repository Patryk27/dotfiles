PROMPT='
%{$fg[yellow]%}%~%{$reset_color%}$(git_prompt_info)$(timer_info)
%(?,%{$fg[green]%};%{$reset_color%},%{$fg[red]%}!%{$reset_color%}) '

ZSH_THEME_GIT_PROMPT_PREFIX=" | %{$fg_bold[blue]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} ✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%} ✔%{$reset_color%}"

function timer_info {
  echo "${timer_msg}"
}

function timer_start() {
  timer=$SECONDS
}

function timer_update() {
  if [ $timer ]; then
    elapsed=$(($SECONDS - $timer))
    timer_msg=" | %{$fg[blue]%}${elapsed}s%f"
    unset timer
  else
    timer_msg=""
  fi
}

autoload -Uz add-zsh-hook
add-zsh-hook preexec timer_start
add-zsh-hook precmd timer_update

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#000000,bg=#e0cec2,underline"

export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
 --color=fg:#605a52,bg:#e8e0d3,hl:#75a3ff
 --color=fg+:#ffffff,bg+:#cfbca0,hl+:#75a3ff
 --color=info:#afaf87,prompt:#d7005f,pointer:#75a3ff
 --color=marker:#87ff00,spinner:#75a3ff,header:#87afaf'
