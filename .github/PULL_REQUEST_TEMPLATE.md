<!-- Enter details of the change here. Include additional tests that have been done, reference to the issue for tracking, etc. -->

<!-- Please check the completed items below -->
### PR checklist
 
- [ ] Read the [contribution guidelines](https://github.com/openapitools/openapi-generator/blob/master/CONTRIBUTING.md).
- [ ] Run the following to [build the project](https://github.com/OpenAPITools/openapi-generator#14---build-projects) and update samples:
  ```
  ./mvnw clean package || exit
  ./bin/generate-samples.sh ./bin/configs/*.yaml || exit
  ./bin/utils/export_docs_generators.sh || exit
  ``` 
  (For Windows users, please run the script in [WSL](https://learn.microsoft.com/en-us/windows/wsl/install))
  Commit all changed files. 
  This is important, as CI jobs will verify _all_ generator outputs of your HEAD commit as it would merge with master. 
  These must match the expectations made by your contribution. 
  You may regenerate an individual generator by passing the relevant config(s) as an argument to the script, for example `./bin/generate-samples.sh bin/configs/java*`. 
  IMPORTANT: Do **NOT** purge/delete any folders/files (e.g. tests) when regenerating the samples as manually written tests may be removed.
- [ ] If your PR is targeting a particular programming language, @mention the [technical committee](https://github.com/openapitools/openapi-generator/#62---openapi-generator-technical-committee) members, so they are more likely to review the pull request.
