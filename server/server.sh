#!/bin/sh
# shellcheck disable=SC2164
cd "${0%/*}"
erl -make
Number=0

if [ "$#" -eq 0 ]; then
    erl -s server -sname main -noshell -setcookie 82736
elif [ "$#" -eq 2 ]; then
  if [ "$1" = "join" ]; then
    # shellcheck disable=SC2039
    erl -eval "server:join('$2')" -sname server"$((++Number))" -noshell -setcookie 82736
  else
    echo "Function $1 does not exist!"
    exit 0
  fi
else
  echo "\Usage: ./server.sh <function> <argument>..."
  exit 0
fi
