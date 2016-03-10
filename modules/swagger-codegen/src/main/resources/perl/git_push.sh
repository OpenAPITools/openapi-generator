#!/bin/sh

# ref: https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/

# Initialize the local directory as a Git repository
git init

# Adds the files in the local repository and stages them for commit.
git add .

# Commits the tracked changes and prepares them to be pushed to a remote repository. 
git commit -m "{{{releaseNote}}}""

# Sets the new remote
# The fatal error can be igored if the remote was added before
if [ "$GIT_TOKEN" = "" ]; then
    echo "[INFO] \$GIT_TOKEN is not set. Using the git crediential in your environment."
    git remote add origin https://github.com/{{{githUserId}}}/{{{gitRepoId}}}.git
else:
    git remote add origin https://{{{gitUserId}}}:${GIT_TOKEN}@github.com/{{{gitUserId}}}/{{{gitRepoId}}}.git
fi

# Pushes (Forces) the changes in the local repository up to the remote repository
git push origin master --force

