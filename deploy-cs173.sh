#!/bin/bash

if [ `hostname -d` != "cs.brown.edu" ]; then
  echo "You must deploy from a cs.brown.edu machine."
  exit 1
fi;

set DESTDIR = csadmin@cs173.cs.brown.edu:/home/csadmin/inst/

./Setup.lhs configure --user --prefix=/tourney && \
./Setup.lhs build && \
./Setup.lhs copy --destdir=cs173 && \
rsync -rltovz --copy-links --delete cs173 $DESTDIR
	
