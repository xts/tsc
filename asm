#!/bin/bash

QUIET=
if [[ "$1" = "-q" ]]; then
   QUIET="-v0"
   shift
fi

if [[ "$1" = "" ]]; then
   echo "usage: $0 [-q] '(code goes here)'"
   exit 1
fi

FILE=`mktemp`
echo $1 > $FILE && cabal $QUIET run tsc -- $FILE --emit-asm