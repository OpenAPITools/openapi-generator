### PR checklist

- [ ] Read the [contribution guidelines](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md).
- [ ] Ran the shell script under `./bin/` to update Petstore sample so that CIs can verify the change. (For instance, only need to run `./bin/{LANG}-petstore.sh` and `./bin/security/{LANG}-petstore.sh` if updating the {LANG} (e.g. php, ruby, python, etc) code generator or {LANG} client's mustache templates). Windows batch files can be found in `.\bin\windows\`.
- [ ] Filed the PR against the correct branch: `3.0.0` branch for changes related to OpenAPI spec 3.0. Default: `master`.
- [ ] Copied the [technical committee](https://github.com/swagger-api/swagger-codegen/#swagger-codegen-technical-committee) to review the pull request if your PR is targeting a particular programming language.

### Description of the PR

(details of the change, additional tests that have been done, reference to the issue for tracking, etc)

