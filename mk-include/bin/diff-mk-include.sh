#!/bin/bash
set -e
# check if PR already existed
if gh pr list -B "${MASTER_BRANCH}" | grep "${MK_INCLUDE_UPDATE_COMMIT_MESSAGE}" ; then
    echo "there is already a mk-include update PR opened"
else
    echo "there is no mk-include update pr currently"

    #preserve current branch
    if [[ $(git branch --show-current) ]]; then
        GIT_PRESERVED_BRANCH=$(git branch --show-current)
    else
        GIT_PRESERVED_BRANCH=$(git rev-parse HEAD)
        ${GIT} checkout -b "${GIT_PRESERVED_BRANCH}"
    fi

    # fetch current master branch
    ${GIT} fetch "${GIT_REMOTE_NAME}" "${MASTER_BRANCH}"
    ${GIT} checkout -b "${MK_INCLUDE_UPDATE_BRANCH}" FETCH_HEAD

    # compare git hash
    if grep -q -e "${MK_INCLUDE_GIT_HASH}" "${MK_INCLUDE_GIT_HASH_LOCATION}" ; then
        echo "mk-include is already at newest pinned version"
    else
        ${MAKE} trigger-mk-include-update-pr || true
    fi

    # switch back to previous preserved branch
    ${GIT} checkout "${GIT_PRESERVED_BRANCH}"
fi