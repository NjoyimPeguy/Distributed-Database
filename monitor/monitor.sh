#!/bin/sh
cd "${0%/*}" || exit
erl -make
. ../config.txt

erl -s monitor -sname monitor -noshell -setcookie "$COOKIE"