PROMPT='
%{$fg[yellow]%}%~%{$reset_color%}$(tt_prompt_info)
%(?,%{$fg[green]%};%{$reset_color%},%{$fg[red]%}!%{$reset_color%}) '

tt_prompt_info() {
  echo -n " %{$FG[244]%}["

  if [ $tt_elapsed ]; then
    echo -n "${tt_elapsed}s|"
  fi

  echo -n "$(date +%H:%M:%S)"
  echo "]%f"
}

tt_start() {
  tt_pending=$SECONDS
}

tt_update() {
  if [ $tt_pending ]; then
    tt_elapsed=$(($SECONDS - $tt_pending))
    unset tt_pending
  else
    tt_elapsed=""
  fi
}

autoload -Uz add-zsh-hook
add-zsh-hook preexec tt_start
add-zsh-hook precmd tt_update
