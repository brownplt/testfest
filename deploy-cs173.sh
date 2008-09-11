#!/bin/bash

if [ `hostname -d` != "cs.brown.edu" ]; then
  echo "You must deploy from a cs.brown.edu machine."
  exit 1
fi;

rsync -rltovz --copy-links --delete \
  cs173 \
  csadmin@cs173.cs.brown.edu:/home/csadmin/inst/
