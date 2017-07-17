### PR checklist

- [ ] Read the [contribution guidelines](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md).
- [ ] Ran the shell script under `./bin/` to update Petstore sample so that CIs can verify the change. (For instance, only need to run `./bin/{LANG}-petstore.sh` and `./bin/security/{LANG}-petstore.sh` if updating the {LANG} (e.g. php, ruby, python, etc) code generator or {LANG} client's mustache templates). Windows batch files can be found in `.\bin\windows\`.
- [ ] Filed the PR against the correct branch: master for non-breaking changes and `3.0.0` branch for breaking (non-backward compatible) changes.

### Description of the PR

(details of the change, additional tests that have been done, reference to the issue for tracking, etc)

