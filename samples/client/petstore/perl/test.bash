#!/bin/bash

set -e

cpanm --installdeps .
perl tests/01_pet_api.t
