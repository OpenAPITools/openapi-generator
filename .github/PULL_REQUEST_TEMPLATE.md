### PR checklist

- [ ] Read the [contribution guildelines](https://github.com/swagger-api/swagger-codegen/blob/master/CONTRIBUTING.md).
- [ ] Ran the shell/batch script under `./bin/` to update Petstore sample so that CIs can verify the change. (NOTE: no need to update *all* Petstore sample. For instance, only need to update PHP Petstore sample by running `./bin/php-petstore.sh` if updating the PHP code generator or PHP client's mustache templates)
- [ ] Filed the PR against the correct branch: master for non-breaking changes and `2.3.0` branch for breaking (non-backward compatible) changes.

### Description of the PR

(details of the change, additional tests that have been done, reference to the issue for tracking, etc)

