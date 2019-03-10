#!/bin/bash

set -e

sudo Rscript -e 'install.packages("jsonlite", repos="https://cran.cnr.berkeley.edu/")'
sudo Rscript -e 'install.packages("httr", repos="https://cran.cnr.berkeley.edu/")'
sudo Rscript -e 'install.packages("testthat", repos="https://cran.cnr.berkeley.edu/")'
sudo Rscript -e 'install.packages("R6", repos="https://cran.cnr.berkeley.edu/")'
sudo Rscript -e 'install.packages("caTools", repos="https://cran.cnr.berkeley.edu/")'

#Packages required but not available: ‘jsonlite’ ‘httr’ ‘R6’ ‘caTools’
#Package suggested but not available: ‘testthat’

R CMD build .
R CMD check *tar.gz
R CMD INSTALL *tar.gz
