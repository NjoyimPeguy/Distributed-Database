#!/bin/sh
alias erl=erl.exe

# shellcheck disable=SC2164
cd "${0%/*}"
erl -make
if [ "$#" -eq 2 ]; then
  if [ "$1" = "start_connection" ]; then
  erl -eval "client:start_connection('$2')" -sname client -noshell -setcookie 82736
  else
      echo "Usage: ./client.sh <connect> <node>"
      exit 0
  fi
  else
    echo "Wrong number of arguments!"
    exit 0
fi