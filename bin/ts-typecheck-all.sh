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
    local selection_file
    selection_file=$(mktemp)
    trap 'rm -f -- "$selection_file"' EXIT

    node - "$root_dir" "$@" > "$selection_file" <<'NODE'
const {execFileSync} = require('child_process');
const fs = require('fs');
const nativePath = require('path');
const path = require('path').posix;
const [root, ...args] = process.argv.slice(2);
const tracked = new Set(execFileSync('git', ['ls-files', '-z', '--', 'samples'], {
    cwd: root, encoding: 'utf8', maxBuffer: 32 * 1024 * 1024
}).split('\0').filter(Boolean));
const eligible = new Set([...tracked].filter(file => file.endsWith('/tsconfig.json'))
    .map(file => path.dirname(file))
    .filter(dir => fs.existsSync(nativePath.join(root, dir, '.openapi-generator-ignore')) &&
        fs.existsSync(nativePath.join(root, dir, 'package.json'))));
let selected = [...eligible].sort();
if (args.length) {
    if (args.length !== 2 || args[0] !== '--samples-json') {
        throw new Error('Usage: ts-typecheck-all.sh [--samples-json \'["sample/path"]\']');
    }
    selected = JSON.parse(args[1]);
    if (!Array.isArray(selected) || selected.some(dir => typeof dir !== 'string' || !eligible.has(dir))) {
        throw new Error('Selection must contain only eligible tracked TypeScript sample directories');
    }
}
for (const dir of new Set(selected)) process.stdout.write(`${dir}\0`);
NODE

    while IFS= read -r -d '' dir; do
        log "➤ ${dir}"
        pushd "${root_dir}/${dir}" > /dev/null
        npm_install \
            || npm_install --force # --force because we have some incompatible peer-dependencies that can't be fixed
        npm exec --package=typescript@5.6.3 --yes -- tsc --noEmit
        log "✓ ${dir}"
        log
        popd > /dev/null
    done < "$selection_file"
    rm -f -- "$selection_file"
    trap - EXIT
}

main "$@"
