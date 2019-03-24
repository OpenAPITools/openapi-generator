### PR checklist

- [ ] Read the [contribution guidelines](https://github.com/openapitools/openapi-generator/blob/master/CONTRIBUTING.md).
- [ ] Ran the shell script under `./bin/` to update Petstore sample so that CIs can verify the change. (For instance, only need to run `./bin/{LANG}-petstore.sh`, `./bin/openapi3/{LANG}-petstore.sh` if updating the {LANG} (e.g. php, ruby, python, etc) code generator or {LANG} client's mustache templates). Windows batch files can be found in `.\bin\windows\`.
- [ ] Filed the PR against the [correct branch](https://github.com/OpenAPITools/openapi-generator/wiki/Git-Branches): `master`~~, `3.4.x`, `4.0.x`~~. Default: `master`.
- [ ] Copied the [technical committee](https://github.com/openapitools/openapi-generator/#62---openapi-generator-technical-committee) to review the pull request if your PR is targeting a particular programming language.

### Description of the PR

(details of the change, additional tests that have been done, reference to the issue for tracking, etc)

