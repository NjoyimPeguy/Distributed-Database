#!/bin/sh
alias erl=erl.exe

# shellcheck disable=SC2164
cd "${0%/*}"
erl -make
if [ "$#" -lt 1 ]; then
    echo "usage: client.sh <node>"
    exit 0
    else
      erl.exe -eval "client:start_connection('$1')" -sname client -noshell -setcookie 82736
fi