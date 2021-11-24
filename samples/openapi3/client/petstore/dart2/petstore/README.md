# Background 

## Current state of tests 

TL;DR currently the only tests are e2e tests that were adapted to use a faked http client. While pushing data around as a smoke test has some value, more testing is required. In particular we need comprehensive unit/integration tests.

- an old set of e2e tests are skipped for CI, as they hit a live endpoint and so are inherently flaky 
  - `pet_test.dart`
  - `store_test.dart`
  - `user_test.dart`
- the above set of tests were adapted to use a faked http client 
  - the tests are not really well suited to being used with a stubbed client, many are basically just testing the endpoint logic
  - while not a great set of tests, they do have some value as a smoke test for template changes
- the adapted tests and files that contain test data:
  - `pet_test_fake_client.dart` 
  - `store_test_fake_client.dart`
  - `user_test_fake_client.dart`
  - `fake_client.dart`
  - `file_upload_response.json`

## Assumptions 

- the tests will be run as part of CI and so have access to dart:io 

# Running 

## If not already done, resolve dependencies

`pub get`

## To run tests in a single file:

`pub run test test/pet_test.dart`

## To run all tests in the test folder:

`pub run test`
