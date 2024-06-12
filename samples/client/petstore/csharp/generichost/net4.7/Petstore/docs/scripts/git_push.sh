#!/bin/sh
# ref: https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/
#
# Usage example: /bin/sh ./git_push.sh wing328 openapi-petstore-perl "minor update" "gitlab.com"

git_user_id=${1:-GIT_USER_ID}
git_repo_id=${2:-GIT_REPO_ID}
release_note=${3:-Minor update}
git_host=${4:-github.com}

starting_directory=$(pwd)
script_root="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $script_root
cd ../..

if [ "$release_note" = "" ] || [ "$release_note" = "Minor update" ]; then
    # it seems unlikely that we would want our git commit message to be the default, so lets prompt the user
    echo "Please provide a commit message or press enter"
    read user_input
    release_note=$user_input
    if [ "$release_note" = "" ]; then
        release_note="no message provided"
    fi
fi

git init
git add .
git commit -am "$release_note"
branch_name=$(git rev-parse --abbrev-ref HEAD)
git_remote=$(git remote)

if [ "$git_remote" = "" ]; then # git remote not defined

    if [ "$GIT_TOKEN" = "" ]; then
        echo "[INFO] \$GIT_TOKEN (environment variable) is not set. Using the git credential in your environment."
        git remote add origin https://${git_host}/${git_user_id}/${git_repo_id}.git
    else
        git remote add origin https://${git_user_id}:"${GIT_TOKEN}"@${git_host}/${git_user_id}/${git_repo_id}.git
    fi

fi

echo "[INFO] Pulling from https://${git_host}/${git_user_id}/${git_repo_id}.git"
git pull origin $branch_name --ff-only

echo "[INFO] Pushing to https://${git_host}/${git_user_id}/${git_repo_id}.git"
git push origin $branch_name

cd $starting_directory
