# PHP client for Wordnik.com API

## Overview

This is a PHP client for the Wordnik.com v4 API. For more information, see http://developer.wordnik.com/ .

## Generation

This client was generated using the provided script:

```
/bin/php-wordnik-api.sh
```

## Testing

These tests require PHPUnit. If you require PHPUnit to be installed, first get PEAR:

```sh
wget http://pear.php.net/go-pear.phar
php -d detect_unicode=0 go-pear.phar
```

Then install PHPUnit:

```sh
pear config-set auto_discover 1
pear install pear.phpunit.de/PHPUnit
```

The tests require you to set three environment varibales:

```sh
export API_KEY=your api key
export USER_NAME=some wordnik.com username
export PASSWORD=the user's password
```

The tests can be run as follows:

```sh
phpunit tests/AccountApiTest.php
phpunit tests/WordApiTest.php
phpunit tests/WordsApiTest.php
phpunit tests/WordListApiTest.php
phpunit tests/WordListsApiTest.php
```
