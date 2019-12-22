#!/bin/sh
alias erl=erl.exe
if [ "$#" -lt 2 ]; then
    echo "usage: deploy.sh <host> <id> [existing-node]"
    exit 0
fi

# shellcheck disable=SC2164
cd "${0%/*}"

NAME=$1
HOST=$2
ACTIVE_NODE=$3

echo "deploying new node $NAME@$HOST..."
erl -make
if [ "$#" -eq 2 ]; then
  erl -sname $NAME -noshell -setcookie 82736 -detached -s server
elif [ "$#" -eq 3 ]; then
  erl -sname $NAME -noshell -setcookie 82736 -detached -eval "server:join('$ACTIVE_NODE')."
else
  echo "Invalid Argument!"
fi