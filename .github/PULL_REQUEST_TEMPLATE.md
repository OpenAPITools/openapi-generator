<!-- Enter details of the change here. Include additional tests that have been done, reference to the issue for tracking, etc. -->

<!-- Please check the completed items below -->
### PR checklist
 
- [ ] Read the [contribution guidelines](https://github.com/openapitools/openapi-generator/blob/master/CONTRIBUTING.md).
- [ ] Pull Request title clearly describes the work in the pull request and Pull Request description provides details about how to validate the work. Missing information here may result in delayed response from the community.
- [ ] Run the following to [build the project](https://github.com/OpenAPITools/openapi-generator#14---build-projects) and update samples:
  ```
  ./mvnw clean package 
  ./bin/generate-samples.sh ./bin/configs/*.yaml
  ./bin/utils/export_docs_generators.sh
  ``` 
  Commit all changed files. 
  This is important, as CI jobs will verify _all_ generator outputs of your HEAD commit as it would merge with master. 
  These must match the expectations made by your contribution. 
  You may regenerate an individual generator by passing the relevant config(s) as an argument to the script, for example `./bin/generate-samples.sh bin/configs/java*`. 
  For Windows users, please run the script in [Git BASH](https://gitforwindows.org/).
- [ ] File the PR against the [correct branch](https://github.com/OpenAPITools/openapi-generator/wiki/Git-Branches): `master` (upcoming 7.1.0 minor release - breaking changes with fallbacks), `8.0.x` (breaking changes without fallbacks)
- [ ] If your PR is targeting a particular programming language, @mention the [technical committee](https://github.com/openapitools/openapi-generator/#62---openapi-generator-technical-committee) members, so they are more likely to review the pull request.
