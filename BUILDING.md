# Building

## Building with Maven

This project is built with maven.

`mvn package`

## Building on GitHub

Every push to GitHub triggers a new build via the `mavenBuild` GitHub Actions workflow. The resulting snapshot version depends on the branch — master builds produce `latest-SNAPSHOT`, while pull request builds produce `pr[number]-SNAPSHOT`.

## Releasing

To release, simply create a tag:

Format: `vX.Y.Z-smalsN` — where `X.Y.Z` is the OpenAPI Generator version this release is based on, and `N` is the incremental release number.

```
git tag -a v1.0.0-smals1 -m "release v1.0.0-smals1"
git push origin v1.0.0-smals1
```

This triggers a new GitHub Actions build (mavenRelease workflow) using the tag as version number and creates release.

The GitHub Action also publishes the artifacts to [GitHub packages](https://github.com/orgs/smals-belgium/packages) and these are added to artifactory through a proxy.

If the release build fails for some reason, you can restart it by re-tagging:

```
git tag -d v1.0.0-smals1
git push --delete origin v1.0.0-smals1
git tag -a v1.0.0-smals1 -m "release v1.0.0-smals1" 
git push origin v1.0.0-smals1
```