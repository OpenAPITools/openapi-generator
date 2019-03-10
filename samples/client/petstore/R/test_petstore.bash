#!/bin/bash

set -e

REPO=http://cran.revolutionanalytics.com

sudo Rscript -e "install.packages('jsonlite', repos='$REPO')"
sudo Rscript -e "install.packages('httr', repos='$REPO')"
sudo Rscript -e "install.packages('testthat', repos='$REPO')"
sudo Rscript -e "install.packages('R6', repos='$REPO')"
sudo Rscript -e "install.packages('caTools', repos='$REPO')"

R CMD build .
R CMD check *tar.gz
R CMD INSTALL *tar.gz
