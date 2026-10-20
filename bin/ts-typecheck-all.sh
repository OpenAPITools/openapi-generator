#!/usr/bin/env bash

set -euo pipefail

log() {
    echo "$@" >&2
}

npm_install() {
    # --ignore-scripts because we don't want to run any pre- or postinstall scripts
    # --no-package-lock because we don't want to update or create the package-lock.json
    # --no-fund because we don't want to check for funding
    # --no-audit because we don't want to run an audit
    # --suppress-warnings because we don't want to see any warnings whilst type checking
    npm i \
        --suppress-warnings \
        --ignore-scripts \
        --no-package-lock \
        --no-fund \
        --no-audit \
        "$@"
}

main() {
    local root_dir
    root_dir=$(git rev-parse --show-toplevel)
    local dir

    for dir in $(git ls-files samples | grep 'tsconfig.json$' | xargs -n1 dirname | sort -u); do
        if [[ ! -f "${root_dir}/${dir}/.openapi-generator-ignore" ]]; then
            # This is not a generated sample; skip it
            continue
        fi
        if [[ ! -f "${root_dir}/${dir}/package.json" ]]; then
            # we can't really guarantee that all dependencies are there to do a typecheck...
            continue            
        fi
        log "➤ ${dir}"
        pushd "${root_dir}/${dir}" > /dev/null
        npm_install \
            || npm_install --force # --force because we have some incompatible peer-dependencies that can't be fixed
        npm exec --package=typescript@5.6.3 --yes -- tsc --noEmit
        log "✓ ${dir}"
        log
        popd > /dev/null
    done
}

main "$@"
