# Changes in PHPUnit 4.0

## PHPUnit 4.0.20

* Fixed [#1242](https://github.com/sebastianbergmann/phpunit/issues/1242): `--self-update` uses OpenSSL API that is deprecated in PHP 5.6

## PHPUnit 4.0.19

* Fixed [#1193](https://github.com/sebastianbergmann/phpunit/issues/1193): Process isolation does not work correctly when PHPUnit is used from PHAR
* Fixed a BC break related to comparing `DOMNode` objects that was introduced in PHPUnit 4.0.18

## PHPUnit 4.0.18

* Fixed [#1218](https://github.com/sebastianbergmann/phpunit/issues/1218): `--self-update` destroys symlink

## PHPUnit 4.0.17

* [Display a message that the PEAR installation method is no longer supported when PHPUnit was installed using the PEAR Installer](https://github.com/sebastianbergmann/phpunit/commit/70b02c6be0176ab8ad3d3c9ec97480556c5dd63b)

## PHPUnit 4.0.16

* [Fixed handling of the `--report-useless-tests`, `--strict-coverage`, `--disallow-test-output`, and `--enforce-time-limit` options](https://github.com/sebastianbergmann/phpunit/commit/38baa9670711adedfe44ef24a33b568f61f3f045)

## PHPUnit 4.0.15

* New release of PHPUnit as PHAR and PEAR package with updated dependencies

## PHPUnit 4.0.14

* New release of PHPUnit as PHAR and PEAR package with updated dependencies

## PHPUnit 4.0.13

* New release of PHPUnit as PHAR and PEAR package with updated dependencies

## PHPUnit 4.0.12

* [Use blacklist from PHP_CodeCoverage](https://github.com/sebastianbergmann/phpunit/commit/16152ba4b8d0104ce34f60cb71b2b982ba84c898)

## PHPUnit 4.0.11

* [Fixed unintended autoloader invokation triggered by the `@beforeClass` and `@afterClass` annotations](https://github.com/sebastianbergmann/phpunit/commit/f12e10fddc3ccbddb652a04d9036aeb5a6d54bff)

## PHPUnit 4.0.10

* New release of PHPUnit as PHAR and PEAR package with updated dependencies (most notably a [fix](https://github.com/sebastianbergmann/phpunit-mock-objects/commit/c5e6274b8f2bf983cf883bb375cf44f99aff200e) in the mock object generator that caused a [performance regression](https://github.com/sebastianbergmann/phpunit/issues/1187))

## PHPUnit 4.0.9

* The "No code coverage will be generated." message was displayed even when code coverage reporting was not requested
* Optimized the search for the `@before`, `@after`, `@beforeClass` and `@afterClass` annotations
* Optimized the usage of `SebastianBergmann\Environment\Runtime::canCollectCodeCoverage()`

## PHPUnit 4.0.8

* Fixed [#1186](https://github.com/sebastianbergmann/phpunit/issues/1186): `@before` and `@after` methods are not called in `@dataProvider` methods

## PHPUnit 4.0.7

* Removed dependency on `phpunit/dbunit` in `composer.json` that was unintentionally added in PHPUnit 4.0.6

## PHPUnit 4.0.6

* New release of PHPUnit as PHAR and PEAR package with updated dependencies

## PHPUnit 4.0.5

* New release of PHPUnit as PHAR and PEAR package with updated dependencies

## PHPUnit 4.0.4

* Fixed stacktrace filtering when PHPUnit is used from a PHAR

## PHPUnit 4.0.3

* New release of PHPUnit as PHAR and PEAR package with updated dependencies

## PHPUnit 4.0.2

*  Fixed an issue related to displaying PHPUnit's version number

## PHPUnit 4.0.1

* Fixed collection of code coverage data for tests that use a data provider

## PHPUnit 4.0.0

* Implemented #382: Added the `$options` parameter to `PHPUnit_Framework_TestCase::getMockFromWsdl()` for configuring the `SoapClient`
* Implemented #628: Added `PHPUnit_Framework_Assert::countOf(), a shortcut to get a `PHPUnit_Framework_Constraint_Count` instance
* Implemented #711: `coverage-text` now has an XML `showOnlySummary` option
* Implemented #719: The `--stderr` switch now respects `--colors` and `--debug`
* Implemented #746: Allow identity checking for non-object types in all asserts that depend on `TraversableContains`
* Implemented #758: Show a proper stack trace when @expectedException fails due to a unexpected exception being thrown
* Implemented #773: Recursive and repeated arrays are more gracefully when comparison differences are exported
* Implemented #813: Added `@before`, `@after`, `@beforeClass` and `@afterClass` annotations
* Implemented #834: Added the `@requires OS` annotation
* Implemented #835: Printers that extend `PHPUnit_TextUI_ResultPrinter` should have similar construction
* Implemented #838: Added a base test listener
* Implemented #859: Added PHP label validation to attribute assertions
* Implemented #869: Added support for the adjacent sibling selector (+) to `PHPUnit_Util_XML::findNodes()`
* Implemented #871: Add Comparator for DateTime objects
* Implemented #877: Added new HTML5 tags to `PHPUnit_Util_XML::findNodes()`
* Added `--coverage-crap4j` switch to generate code coverage report in Crap4J XML format
* `assertCount()`, `assertNotCount()`, `assertSameSize()`, and `assertNotSameSize()` now support all objects that implement the `Traversable` interface
* A test will now fail in strict mode when it uses the `@covers` annotation and code that is not expected to be covered is executed
* All relative paths in a configuration file are now resolved relative to that configuration file
* Fixed #240: XML strings are escaped by removing invalid characters
* Fixed #261: `setUp()` and `setUpBeforeClass()` are run before filters are applied
* Fixed #541: Excluded groups are counted towards total number of tests being executed
* Fixed #789: PHP INI settings would not be passed to child processes
* Fixed #806: Array references are now properly displayed in error output
* Fixed #808: Resources are now reported as `resource(13) of type (stream)` instead of `NULL`
* Fixed #873: PHPUnit suppresses exceptions thrown outside of test case function
* Fixed: `phpt` test cases now use the correct php binary when executed through wrapper scripts

