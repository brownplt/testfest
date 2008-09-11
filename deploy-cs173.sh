#!/bin/bash

if [ `hostname -d` != "cs.brown.edu" ]; then
  echo "You must deploy from a cs.brown.edu machine."
  exit 1
fi;

rsync -rltovz --copy-links --delete \
  ./static \
  csadmin@cs173.cs.brown.edu:/home/csadmin/tourney
rsync -rltovz --safe-links --delete \
  ./dist/build/tourney-server/tourney-server \
  csadmin@cs173.cs.brown.edu:/home/csadmin/tourney
rsync -rltovz --safe-links --delete \
  ./dist/build/cs173tourney-accounts/cs173tourney-accounts \
  csadmin@cs173.cs.brown.edu:/home/csadmin/tourney
rsync -rltovz --safe-links --delete \
  ./run csadmin@cs173.cs.brown.edu:/home/csadmin/tourney
ssh csadmin@cs173.cs.brown.edu  chmod -R a+rX /home/csadmin/tourney

