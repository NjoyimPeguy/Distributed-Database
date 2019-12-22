#!/bin/sh
alias erl=erl.exe
# shellcheck disable=SC2164
cd "${0%/*}"
erl -make

if [ "$#" -eq 0 ]; then
    erl -s server -sname main -noshell -setcookie 82736
elif [ "$#" -eq 2 ]; then
  if [ "$1" = "join" ]; then
    erl -eval "server:join('$2')" -sname server -noshell -setcookie 82736
  else
    echo "Function $1 does not exist!"
    exit 0
  fi
else
  echo "\Usage: ./server.sh <function> <argument>..."
  exit 0
fi
