# Changes in PHPUnit 4.4

## PHPUnit 4.4.5

* Fixed [#1592](https://github.com/sebastianbergmann/phpunit/issues/1592): Incorrect dependency information

## PHPUnit 4.4.4

* Fixed [#1587](https://github.com/sebastianbergmann/phpunit/issues/1587): Class `SebastianBergmann\Exporter\Context` not found

## PHPUnit 4.4.3

* New PHAR release due to updated dependencies

## PHPUnit 4.4.2

* Merged [#1567](https://github.com/sebastianbergmann/phpunit/issues/1567): `coverage-crap4j` missing in XSD for PHPUnit XML configuration
* Merged [#1573](https://github.com/sebastianbergmann/phpunit/issues/1573): Updates for the XSD for PHPUnit XML configuration
* Fixed [#1570](https://github.com/sebastianbergmann/phpunit/issues/1570): Test that prints output is marked as failure and not as risky when `--disallow-test-output` is used
* Fixed `--stderr` with `--tap` or `--testdox` options

## PHPUnit 4.4.1

* Merged [#1528](https://github.com/sebastianbergmann/phpunit/issues/1528): Add `expectedCount()` to `toString()` return value

## PHPUnit 4.4.0

* Merged [#1371](https://github.com/sebastianbergmann/phpunit/issues/1371): Implement `assertArraySubset()` assertion
* Merged [#1427](https://github.com/sebastianbergmann/phpunit/issues/1427): Improve failure output for tests when provided data is binary
* Merged [#1439](https://github.com/sebastianbergmann/phpunit/issues/1439): Add support for `double` to `assertInternalType()`
* Merged [#1458](https://github.com/sebastianbergmann/phpunit/issues/1458): Only enable colors when PHPUnit is run on a console (and output is not sent to a file)

