#!/usr/bin/env bash

bin_path=$1
args=$2
if [[ -z $bin_path || -z $args ]]; then
	echo "both path to binary and args must be provided"
	exit 1
fi
exec dlv --listen=0.0.0.0:2345 --headless=true --api-version=2 --check-go-version=false exec $bin_path -- $args
