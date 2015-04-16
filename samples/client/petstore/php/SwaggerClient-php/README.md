## Requirements

PHP 5.3.3 and later.

## Composer

You can install the bindings via [Composer](http://getcomposer.org/). Add this to your `composer.json`:

    {
        "repositories": [
        {
            "type": "git",
            "url": "https://github.com/wing328/SwaggerPetstore-php.git"
        }
        ],
        "require": {
            "SwaggerPetstore/SwaggerPetstore-php": "*@dev"
        }
    }

Then install via:

    composer install

To use the bindings, use Composer's [autoload](https://getcomposer.org/doc/00-intro.md#autoloading):

    require_once('vendor/autoload.php');

## Manual Installation

If you do not wish to use Composer, you can download the latest release. Then, to use the bindings, include the `SwaggerPetstore.php` file.

    require_once('/path/to/SwaggerPetstore-php/SwaggerPetstore.php');

## Getting Started

php test.php

## Documentation

TODO

## Tests

In order to run tests first install [PHPUnit](http://packagist.org/packages/phpunit/phpunit) via [Composer](http://getcomposer.org/):

    composer update

To run the test suite:

    ./vendor/bin/phpunit tests

