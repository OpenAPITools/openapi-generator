#!/bin/bash

set -e

REPO=https://cloud.r-project.org

export R_LIBS_USER=$HOME/R

echo "R lib directory: $R_LIBS_USER"

mkdir $R_LIBS_USER || true

Rscript -e "install.packages('jsonlite', repos='$REPO', lib='$R_LIBS_USER')"
Rscript -e "install.packages('httr', repos='$REPO', lib='$R_LIBS_USER')"
Rscript -e "install.packages('testthat', repos='$REPO', lib='$R_LIBS_USER')"
Rscript -e "install.packages('R6', repos='$REPO', lib='$R_LIBS_USER')"
Rscript -e "install.packages('base64enc', repos='$REPO', lib='$R_LIBS_USER')"
Rscript -e "install.packages('rlang', repos='$REPO', lib='$R_LIBS_USER')"
Rscript -e "install.packages('rjson', repos='$REPO', lib='$R_LIBS_USER')"
Rscript -e "install.packages('devtools', repos='$REPO', lib='$R_LIBS_USER')"

rm petstore_1.0.0.tar.gz || true

R CMD build .
R CMD check *tar.gz --no-manual
R CMD install --preclean *tar.gz
