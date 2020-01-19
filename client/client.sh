#!/bin/sh
cd "${0%/*}" || exit
erl -make
. ../config.txt
nextId=$CLIENT_ID

if [ "$#" -eq 2 ]; then
  if [ "$1" = "start_connection" ]; then
  erl -eval "client:start_connection('$2')" -sname client"$nextId" -noshell -setcookie "$COOKIE"
  sed -i "s/CLIENT_ID=.*/CLIENT_ID=$((nextId + 1))/" ../config.txt
  else
      echo "Usage: ./client.sh <connect> <node>"
      exit 0
  fi
  else
    echo "Wrong number of arguments!"
    exit 0
fi