#!/bin/sh
cd "${0%/*}" || exit
erl -make
. ./server_ids.txt
nextCounter=$SERVER_ID

if [ "$#" -eq 0 ]; then
    erl -s server -sname main -noshell -setcookie 82736
elif [ "$#" -eq 2 ]; then
  if [ "$1" = "join" ]; then
    erl -eval "server:join('$2')" -sname server"$nextCounter" -noshell -setcookie 82736
    sed -i "s/SERVER_ID=.*/SERVER_ID=$((nextCounter + 1))/" ./server_ids.txt
  else
    echo "Function $1 does not exist!"
    exit 0
  fi
else
  echo "\Usage: ./server.sh <function> <argument>..."
  exit 0
fi
