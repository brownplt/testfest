#!/bin/bash

if [ `hostname -d` != "cs.brown.edu" ]; then
  echo "You must deploy from a cs.brown.edu machine."
  exit 1
fi;

DESTDIR="csadmin@cs173.cs.brown.edu:/home/csadmin/tourney/"

./Setup.lhs configure --user --prefix=/home/csadmin/tourney && \
./Setup.lhs build && \
./Setup.lhs copy --destdir=dist/cs173
rsync -rltovz --copy-links --delete dist/cs173/home/csadmin/tourney/ $DESTDIR
	
