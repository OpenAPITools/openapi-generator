About
-----

PHP\_CodeSniffer is a set of two PHP scripts; the main `phpcs` script that tokenizes PHP, JavaScript and CSS files to detect violations of a defined coding standard, and a second `phpcbf` script to automatically correct coding standard violations. PHP\_CodeSniffer is an essential development tool that ensures your code remains clean and consistent.

[![Build Status](https://travis-ci.org/squizlabs/PHP_CodeSniffer.svg?branch=phpcs-fixer)](https://travis-ci.org/squizlabs/PHP_CodeSniffer) [![Code consistency](http://squizlabs.github.io/PHP_CodeSniffer/analysis/squizlabs/PHP_CodeSniffer/grade.svg)](http://squizlabs.github.io/PHP_CodeSniffer/analysis/squizlabs/PHP_CodeSniffer)

Requirements
------------

PHP\_CodeSniffer requires PHP version 5.1.2 or greater, although individual sniffs may have additional requirements such as external applications and scripts. See the [Configuration Options manual page](https://github.com/squizlabs/PHP_CodeSniffer/wiki/Configuration-Options) for a list of these requirements.

The SVN pre-commit hook requires PHP version 5.2.4 or greater due to its use of the vertical whitespace character.

Installation
------------

The easiest way to get started with PHP\_CodeSniffer is to download the [Phar](http://php.net/manual/en/intro.phar.php) files for each of the commands:

    curl -OL https://squizlabs.github.io/PHP_CodeSniffer/phpcs.phar
    php phpcs.phar -h

    curl -OL https://squizlabs.github.io/PHP_CodeSniffer/phpcbf.phar
    php phpcbf.phar -h

If you use PEAR, you can install PHP\_CodeSniffer using the PEAR installer. This will make the `phpcs` and `phpcbf` commands immediately available for use. To install PHP\_CodeSniffer using the PEAR installer, first ensure you have [installed PEAR](http://pear.php.net/manual/en/installation.getting.php) and then run the following command:

    pear install PHP_CodeSniffer

If you prefer using [Composer](http://getcomposer.org/) you can easily install PHP_CodeSniffer system-wide with the following command:

    composer global require "squizlabs/php_codesniffer=*"

Make sure you have `~/.composer/vendor/bin/` in your PATH.

Or alternatively, include a dependency for `squizlabs/php_codesniffer` in your `composer.json` file. For example:

    {
        "require-dev": {
            "squizlabs/php_codesniffer": "2.*"
        }
    }

You will then be able to run PHP_CodeSniffer from the vendor bin directory:

    ./vendor/bin/phpcs -h
    ./vendor/bin/phpcbf -h

You can also download the PHP\_CodeSniffer source and run the `phpcs` and `phpcbf` commands directly from the Git checkout:

    git clone git://github.com/squizlabs/PHP_CodeSniffer.git
    cd PHP_CodeSniffer
    php scripts/phpcs -h
    php scripts/phpcbf -h

Documentation
-------------

The documentation for PHP\_CodeSniffer is available on the [Github wiki](https://github.com/squizlabs/PHP_CodeSniffer/wiki).

Information about upcoming features and releases is available on the [Squiz Labs blog](http://www.squizlabs.com/php-codesniffer).

Issues
------

Bug reports and feature requests can be submitted on the [Github Issue Tracker](https://github.com/squizlabs/PHP_CodeSniffer/issues) or the [PEAR bug tracker](http://pear.php.net/package/PHP_CodeSniffer/bugs).

Contributing
-------------

See [CONTRIBUTING.md](CONTRIBUTING.md) for information.
