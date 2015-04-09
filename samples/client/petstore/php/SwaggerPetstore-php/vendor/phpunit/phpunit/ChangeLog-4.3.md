# Changes in PHPUnit 4.3

## PHPUnit 4.3.5

* Merged [#1484](https://github.com/sebastianbergmann/phpunit/issues/1484): Removed `lazymap` from blacklist as it is not longer used
* Merged [#1489](https://github.com/sebastianbergmann/phpunit/issues/1489): Do not send output from tests in process isolation when testing output

## PHPUnit 4.3.4

* Fixed [#1428](https://github.com/sebastianbergmann/phpunit/issues/1428): Issue with Composer dependencies
* Fixed [#1447](https://github.com/sebastianbergmann/phpunit/issues/1447): PHPT tests treat line endings inconsistently

## PHPUnit 4.3.3

* Fixed [#1471](https://github.com/sebastianbergmann/phpunit/issues/1471): Output made while test is running is printed although `expectOutputString()` is used when an assertion fails

## PHPUnit 4.3.2

* Fixed [#1468](https://github.com/sebastianbergmann/phpunit/issues/1468): Incomplete and `@todo` annotated tests are counted twice

## PHPUnit 4.3.1

* New release of PHPUnit as PHP Archive (PHAR) with updated dependencies

## PHPUnit 4.3.0

* Merged [#1358](https://github.com/sebastianbergmann/phpunit/issues/1358): Implement `@expectedExceptionMessageRegExp` annotation
* Merged [#1360](https://github.com/sebastianbergmann/phpunit/issues/1360): Allow a test to identify whether it runs in isolation
* Fixed [#1216](https://github.com/sebastianbergmann/phpunit/issues/1216): Bootstrap does not have global variables set when `--bootstrap` is specified on commandline
* Fixed [#1351](https://github.com/sebastianbergmann/phpunit/issues/1351): `TestResult` object contains serialized test class upon test failure/exception in process isolation
* Fixed [#1437](https://github.com/sebastianbergmann/phpunit/issues/1437): Risky test messages mask failures 

