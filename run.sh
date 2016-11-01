#!/usr/bin/env bash

set -eux

cd $HOME
tar zxfv /secret/home.tar.gz
ssh-keyscan -H github.com >> $HOME/.ssh/known_hosts

source /secret-mirror/config.sh

#git config --global user.email michael+all-cabal-tool@snoyman.com
#git config --global user.name all-cabal-tool

exec /usr/local/bin/all-cabal-tool \
    --username all-cabal-tool \
    --email michael+all-cabal-files@snoyman.com \
    --gpg-sign D6CF60FD \
    --s3-bucket $S3_BUCKET \
    --aws-access-key $AWS_ACCESS_KEY_ID \
    --aws-secret-key $AWS_SECRET_ACCESS_KEY
