#!/bin/bash

set -e

REPO=http://cran.revolutionanalytics.com

R_LIBS_USER=$HOME/R

Rscript -e "install.packages('jsonlite', repos='$REPO')"
Rscript -e "install.packages('httr', repos='$REPO')"
Rscript -e "install.packages('testthat', repos='$REPO')"
Rscript -e "install.packages('R6', repos='$REPO')"
Rscript -e "install.packages('caTools', repos='$REPO')"

R CMD build .
R CMD check *tar.gz --no-manual
R CMD INSTALL *tar.gz
