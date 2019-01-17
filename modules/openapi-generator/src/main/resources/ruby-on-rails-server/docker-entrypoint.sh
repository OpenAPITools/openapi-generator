#!/bin/sh
set -e

if [ "$1" = 'bin/rails' ] && [ "$2" = 's' ]; then
    rm -f /myapp/tmp/pids/server.pid
    bin/rails db:migrate
fi

exec "$@"
