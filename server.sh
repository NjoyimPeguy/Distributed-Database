#!/bin/sh
alias erl=erl.exe
COOKIE=82736
# shellcheck disable=SC2164
cd "${0%/*}"
erl -make

if [ "$1" = "init" ]; then
    erl -eval "server:init()" -sname server -noshell -setcookie $COOKIE
elif [ "$#" -eq 2 ]; then
  if [ "$1" = "join" ]; then
    erl -eval "server:join('$2')" -sname server1 -noshell -setcookie $COOKIE
  else
    echo "wrong function!"
  fi
else
  echo "Wrong arguments!"
fi
