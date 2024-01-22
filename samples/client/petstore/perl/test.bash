#!/bin/bash

set -e

cpanm --local-lib=~/perl5 local::lib && eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
cpanm --installdeps .

perl tests/01_pet_api.t
perl tests/02_store_api.t
perl tests/03_api_factory.t
perl tests/04_role.t
perl tests/06_inheritance.t

