#!/bin/bash -eu

SCRIPT_DIR=$(dirname $0)
cd $SCRIPT_DIR

source pkg_version

./build-deps-rpm

# TODO: Don't hard code
ARCH=x86_64
MOCK_CONFIG="-"
./build-srpm  -i buildsome -o . -m ${MOCK_CONFIG} -d $(ls -1 buildsome-deps-${PKG_VERSION}-${PKG_RELEASE}*${ARCH}.rpm)

