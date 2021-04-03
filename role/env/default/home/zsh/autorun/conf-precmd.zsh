function precmd() {
  echo -n "\033]0;${PWD##*/}\007"
}
