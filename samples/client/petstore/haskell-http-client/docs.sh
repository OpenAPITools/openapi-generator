#!/bin/bash
set -e

cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)

if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

pkg=$(awk -F ":[[:space:]]*" 'tolower($1)=="name"    { print $2 }' < "$cabal_file")
ver=$(awk -F ":[[:space:]]*" 'tolower($1)=="version" { print $2 }' < "$cabal_file")

if [ -z "$pkg" ]; then
  echo "Unable to determine package name"
  exit 1
fi

if [ -z "$ver" ]; then
  echo "Unable to determine package version"
  exit 1
fi

echo "Detected package: $pkg-$ver"

cabal haddock --hyperlink-source --html-location='https://www.stackage.org/haddock/nightly-2017-08-25/$pkg-$version' --contents-location='https://www.stackage.org/nightly-2017-08-25/package/$pkg-$version'

dir="build-docs"
trap 'rm -r "$dir"' EXIT
mkdir -p $dir
cp -R dist/doc/html/$pkg/ $dir/$pkg-$ver-docs
