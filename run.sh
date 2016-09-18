#!/usr/bin/env bash

set -eux

cd $HOME
tar zxfv /secret/home.tar.gz
ssh-keyscan -H github.com >> $HOME/.ssh/known_hosts

git clone git@github.com:commercialhaskell/all-cabal-files --branch hackage --depth=1
git clone git@github.com:commercialhaskell/all-cabal-hashes --branch hackage --depth=1
git clone git@github.com:commercialhaskell/all-cabal-metadata --depth=1

git config --global user.email michael+all-cabal-tool@snoyman.com
git config --global user.name all-cabal-tool

/usr/local/bin/hackage-watcher http://hackage.fpcomplete.com/00-index.tar.gz /usr/local/bin/run-inner.sh
