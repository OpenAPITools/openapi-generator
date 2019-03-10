#!/bin/bash

set -e

sudo Rscript -e 'install.packages("jsonlite", repos="https://cran.rstudio.com")'
sudo Rscript -e 'install.packages("httr", repos="https://cran.rstudio.com")'

R CMD build .
R CMD check *tar.gz
R CMD INSTALL *tar.gz
