#!/usr/bin/env bash

set -eux

cd ~/all-cabal-files
wget http://hackage.fpcomplete.com/00-index.tar.gz
tar xf 00-index.tar.gz
rm -f 00-index.tar.gz
if [ -n "$(git status --porcelain)" ]
then
    git add -A
    git config core.autocrlf input
    git commit -m "Update from Hackage at $(date --utc --iso=sec)" --gpg-sign=D6CF60FD
    git push git@github.com:commercialhaskell/all-cabal-files.git HEAD:master
    git push git@github.com:commercialhaskell/all-cabal-files.git HEAD:hackage

    git tag current-hackage -u D6CF60FD -m "Update from Hackage at $(date --utc --iso=sec)" -f
    git push git@github.com:commercialhaskell/all-cabal-files.git --tags --force
else
    echo No changes present
fi

cd ~/all-cabal-hashes
/usr/local/bin/all-cabal-hashes-tool
if [ -n "$(git status --porcelain)" ]
then
    git add -A
    git commit -m "Update from Hackage at $(date --utc --iso=sec)" --gpg-sign=D6CF60FD
    git pull --rebase
    git push git@github.com:commercialhaskell/all-cabal-hashes.git HEAD:hackage

    git tag current-hackage -u D6CF60FD -m "Update from Hackage at $(date --utc --iso=sec)" -f
    git push git@github.com:commercialhaskell/all-cabal-hashes.git --tags --force
else
    echo No changes present
fi

cd ~/all-cabal-metadata
/usr/local/bin/all-cabal-metadata-tool 50

if [ -n "$(git status --porcelain)" ]
then
    git add -A
    git commit -m "Update from Hackage at $(date --utc --iso=sec)"
    git fetch
    git rebase origin/master
    git push git@github.com:commercialhaskell/all-cabal-metadata HEAD:master
else
    echo No changes present
fi

echo Completed run-inner.sh
date
