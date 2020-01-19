#!/bin/sh
cd "${0%/*}" || exit
erl -make
. ../config.txt

NAME=$1
ACTIVE_NODE=$2

if [ "$#" -eq 1 ]; then
  xterm -hold -e erl -sname "$NAME" -noshell -setcookie "$COOKIE" -s server
elif [ "$#" -eq 2 ]; then
  xterm -e erl -sname "$NAME" -noshell -setcookie "$COOKIE" -detached -eval "server:join('$ACTIVE_NODE')."
else
  echo "usage: deploy.sh name"
  echo "usage: deploy.sh name existingnode"
  exit 0
fi