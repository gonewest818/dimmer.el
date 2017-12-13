#!/bin/bash

# Install cask for Travis CI
# or if already installed, then check for updates

set -x

WORKDIR=${HOME}/local
CASKDIR=$WORKDIR/cask

. travis-ci/retry.sh

upgrade_cask_or_reset() {
    cask upgrade-cask || { rm -rf .cask && false; }
}

if [ -d $CASKDIR ]
then
    travis_retry upgrade_cask_or_reset
else
    git clone https://github.com/cask/cask.git $CASKDIR
    travis_retry upgrade_cask_or_reset
fi
