CHANGELOG
=============

## 0.7.0 (WIP)

- Support environment prop in json_file (issue #15)
- Support commands: push, open, service, last (issue #16)
- Show helpful message if the requirements are not satisfied (issue #24)

## 0.6.1

- Add CLI option: `--exclude-no-stmt` (issue #23)
- Add .coveralls.yml configuration: `exclude_no_stmt` (issue #23)
- Fix issue #27: Response message is not shown if exception occurred

## 0.6

- Support configuration for multiple clover.xml (issue #11)
- Fix issue #12: end of file should not be included in code coverage
- Show exception log at sending a request instead of exception backtrace
- Log enhancement (issue #14): 
    - show file size of `json_file`
    - show number of included source files
    - show elapsed time and memory usage
    - show coverage
    - show response message
- Relax dependent libs version (issue #18)
- Add connection error handling (issue #21)

## 0.5

- `--verbose (-v)` CLI option enables logging
- Fix: only existing file lines should be included in coverage data
- Support standardized env vars ([Codeship](https://www.codeship.io) supported these env vars)
    - CI_NAME
    - CI_BUILD_NUMBER
    - CI_BUILD_URL
    - CI_BRANCH
    - CI_PULL_REQUEST
- Refactor console logging (PSR-3 compliant)
- Change composer's minimal stability from dev to stable

## 0.4

- Replace REST client implementation by [guzzle/guzzle](https://github.com/guzzle/guzzle)
- Change: `repo_token` is required on CircleCI, Jenkins

## 0.3

- Better CLI implementation by using [symfony/Console](https://github.com/symfony/Console) component
- Support `--dry-run`, `--config (-c)` CLI option

## 0.2

- Support .coveralls.yml

## 0.1

- First release
- Support Travis CI (tested)
- Implement CircleCI, Jenkins, local environment (but not tested on these CI environments)
- Collect coverage information from clover.xml
- Collect git repository information

