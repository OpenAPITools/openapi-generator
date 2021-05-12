# Tests for cc-mk-include
-------------------------
this is the CI test for cc-mk-include, the directories contain different build scenarios. Tests use pytest to run commands and assert outputs. To run tests locally, use the cmd in semaphore.yml, you also need required secrets in place.
```
- make copy-mk-include-go-docker-build-test
- cd tests/go-docker-build-test
- make go-docker-build-test
```

## layout
- go docker build test

  Cloud build go&docker workflow, build go on docker image, and push to artifactory. The tests make sure all basic functions will not be broken.

- test_utils.py

    python wrapper used for tests.

## How to add new tests
Add a new folder and name it as whatever build scenario/purpose you want to test, e.g. go-docker-build-test, docker-maven-build-test-2, python-build-test-5.

Put any needed test files in the folder, construct a simple service, and write assert tests in test.py. It should simulate a cloud repo's build process. Use Use `run_cmd()` to run build make cmd and use `assert_in_output` or other assert method to verify output.

Use Toplevel Makefile to copy mk-include to your subdirectory, and run your test cmd.
