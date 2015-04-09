[![Latest Stable Version](https://poser.pugx.org/phpunit/php-code-coverage/v/stable.png)](https://packagist.org/packages/phpunit/php-code-coverage)
[![Build Status](https://travis-ci.org/sebastianbergmann/php-code-coverage.svg?branch=master)](https://travis-ci.org/sebastianbergmann/php-code-coverage)

# PHP_CodeCoverage

**PHP_CodeCoverage** is a library that provides collection, processing, and rendering functionality for PHP code coverage information.

## Requirements

* PHP 5.3.3 is required but using the latest version of PHP is highly recommended
* [Xdebug](http://xdebug.org/) 2.1.3 is required but using the latest version of Xdebug is highly recommended

## Installation

To add PHP_CodeCoverage as a local, per-project dependency to your project, simply add a dependency on `phpunit/php-code-coverage` to your project's `composer.json` file. Here is a minimal example of a `composer.json` file that just defines a dependency on PHP_CodeCoverage 2.0:

    {
        "require": {
            "phpunit/php-code-coverage": "~2.0"
        }
    }

## Using the PHP_CodeCoverage API

```php
<?php
$coverage = new PHP_CodeCoverage;
$coverage->start('<name of test>');

// ...

$coverage->stop();

$writer = new PHP_CodeCoverage_Report_Clover;
$writer->process($coverage, '/tmp/clover.xml');

$writer = new PHP_CodeCoverage_Report_HTML;
$writer->process($coverage, '/tmp/code-coverage-report');
```

