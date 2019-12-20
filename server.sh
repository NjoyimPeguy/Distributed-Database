#!/bin/sh
alias erl=erl.exe
# shellcheck disable=SC2164
cd "${0%/*}"
erl -make

if [ "$#" -eq 2 ]; then
  if [ "$1" = "start" ]; then
    erl -eval "server:start()" -sname "$2" -noshell -setcookie 82736
  elif [ "$1" = "join" ]; then
    erl -eval "server:join('$2')" -sname server$RANDOM -noshell -setcookie 82736
  else
    echo "Function $1 does not exist!"
  fi
else
  echo "Usage: ./server.sh <function> <argument>..."
fi
