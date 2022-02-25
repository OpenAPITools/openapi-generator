---
id: faq-contributing
title: "FAQ: Contributing"
---

## Automated checks on my PR have failed. Do you know what's wrong?

Please do the following:

* Click on the failed tests and check the log to see what's causing the errors.
* If it's related to connection timeout in downloading dependencies, please restart the CI jobs (which can be done by closing and reopening the PR)
* If it's some other reason, please tag someone on the [core team](./core-team.md) for assistance.

## The public petstore server returns status 500, can I run it locally?

Yes, please run the following commands (assuming you've docker installed):

```
docker pull swaggerapi/petstore
docker run -d -e SWAGGER_HOST=http://petstore.swagger.io -e SWAGGER_BASE_PATH=/v2 -p 80:8080 swaggerapi/petstore
docker ps -a
```
 
Then add the following to your local [hosts](https://en.wikipedia.org/wiki/Hosts_%28file%29) table:

```
127.0.0.1    petstore.swagger.io
```

## Who should I report a security vulnerability to?

Please contact team@openapitools.org with the details and we'll follow up with you.


## How can I rebase my PR on the latest master?

Please refer to http://rypress.com/tutorials/git/rebasing, or follow the steps below (assuming the branch for the PR is "fix_issue_9999"):

1. git checkout master
2. git pull upstream master (assuming `upstream` is pointing to the official repo)
3. git checkout fix_issue_9999
4. git rebase master
5. Resolve merge conflicts, if any, and run "git commit -a"
6. Rebase done (you may need to add --force when doing `git push`)

(To setup `upstream` pointing to the official repo, please run `git remote add upstream  https://github.com/openapitools/openapi-generator.git`)

## How can I update commits that are not linked to my GitHub account?

Please refer to https://stackoverflow.com/questions/3042437/how-to-change-the-commit-author-for-one-specific-commit or you can simply add the email address in the commit as your secondary email address in your GitHub account.

## Any useful git tips to share?

Yes, http://www.alexkras.com/19-git-tips-for-everyday-use/

## How can I submit a PR to fix bugs or make enhancements?

Visit https://github.com/openapitools/openapi-generator and then click on the "Fork" button in the upper right corner. Then in your local machine, run the following (assuming your github ID is "your_user_id")

1) git clone https://github.com/your_user_id/openapi-generator.git
2) cd openapi-generator
3) git checkout -b fix_issue9999
4) make changes
5) git commit -a (you may need to use `git add filename` to add new files)
6) git push origin fix_issue9999
7) Visit https://github.com/openapitools/openapi-generator in your browser and click on the button to file a new PR based on fix_issue9999
