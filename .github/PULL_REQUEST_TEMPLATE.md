<!-- Enter details of the change here. Include additional tests that have been done, reference to the issue for tracking, etc. -->

<!-- Please check the completed items below -->
### PR checklist

- [ ] Read the [contribution guidelines](https://github.com/openapitools/openapi-generator/blob/master/CONTRIBUTING.md).
- [ ] If contributing template-only or documentation-only changes which will change sample output, [build the project](https://github.com/OpenAPITools/openapi-generator#14---build-projects) before.
- [ ] Run the shell script(s) under `./bin/` (or Windows batch scripts under`.\bin\windows`) to update Petstore samples related to your fix. This is important, as CI jobs will verify _all_ generator outputs of your HEAD commit, and these must match the expectations made by your contribution. You only need to run `./bin/{LANG}-petstore.sh`, `./bin/openapi3/{LANG}-petstore.sh` if updating the code or mustache templates for a language (`{LANG}`) (e.g. php, ruby, python, etc).
- [ ] File the PR against the [correct branch](https://github.com/OpenAPITools/openapi-generator/wiki/Git-Branches): `master`, `4.3.x`, `5.0.x`. Default: `master`.
- [ ] Copy the [technical committee](https://github.com/openapitools/openapi-generator/#62---openapi-generator-technical-committee) to review the pull request if your PR is targeting a particular programming language.
