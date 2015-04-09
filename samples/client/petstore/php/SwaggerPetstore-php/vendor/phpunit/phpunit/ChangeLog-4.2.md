# Changes in PHPUnit 4.2

## PHPUnit 4.2.5

* New release of PHPUnit as PHP Archive (PHAR) with updated dependencies

## PHPUnit 4.2.4

* Fixed [#1413](https://github.com/sebastianbergmann/phpunit/issues/1413): `assertCount()` hangs in infinite loop on HHVM

## PHPUnit 4.2.3

* Fixed [#1403](https://github.com/sebastianbergmann/phpunit/issues/1403): `phpunit --self-update` does not work

## PHPUnit 4.2.2

* Fixed [#1399](https://github.com/sebastianbergmann/phpunit/issues/1399): `enforceTimeLimit` configuration option is not handled

## PHPUnit 4.2.1

* Fixed [#1380](https://github.com/sebastianbergmann/phpunit/issues/1380): `assertMatch()` returns `Unexpected end tag : hr`
* Fixed [#1390](https://github.com/sebastianbergmann/phpunit/issues/1390): Licensing issue with third-party components bundled in PHAR distribution

## PHPUnit 4.2.0

* Tests annotated with `@todo` will now be reported as risky when the `--disallow-todo-tests` option is used or `beStrictAboutTodoAnnotatedTests=true` is set in the configuration file
* The `atLeast()` and `atMost()` invocation count matchers were added
* The `assertTag()` and `assertSelect*()` assertion methods have been deprecated in favor of the [phpunit-dom-assertions](https://github.com/phpunit/phpunit-dom-assertions) extension; these methods will be removed in PHPUnit 5.0
* `trigger_error(__METHOD__ . ' is deprecated', E_USER_DEPRECATED);` is used now to indicate that a PHPUnit API method is deprecated; the old "system" for deprecating methods has been removed
* The PHP Archive (PHAR) distribution of PHPUnit can now be used as a library; `include()`ing or `require()`ing it will not execute the CLI test runner

