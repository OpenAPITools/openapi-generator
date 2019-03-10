#!/bin/bash

set -e

Rscript -e 'install.packages("jsonlite")'
Rscript -e 'install.packages("httr")'

R CMD build .
R CMD check *tar.gz
R CMD INSTALL *tar.gz
