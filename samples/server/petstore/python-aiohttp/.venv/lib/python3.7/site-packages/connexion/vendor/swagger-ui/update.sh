#!/bin/bash

if [ -z "$1" ]; then
    echo 'Usage: ./update.sh <SWAGGER-UI-VERSION>'
    exit 1
fi

version=$1
archive="https://github.com/swagger-api/swagger-ui/archive/v${version}.tar.gz"

folder="swagger-ui-${version}"
rm -fr "$folder"
mkdir -p "$folder"
wget $archive -O - | tar -xvz -C "$folder" --strip-components=1
rsync -av "$folder/dist/" ./

# apply our patch
patch index.html index.html.patch

rm -fr "$folder"
