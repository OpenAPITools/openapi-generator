<?php

// Unit tests for PHP Wordnik API client.
//
// Requires you to set three environment varibales:
//     API_KEY      your API key
//     USER_NAME    the username of a user
//     PASSWORD     the user's password

// Run all tests:
//
//     phpunit tests/AccountApiTest.php
//     phpunit tests/WordApiTest.php
//     phpunit tests/WordsApiTest.php
//     phpunit tests/WordListApiTest.php
//     phpunit tests/WordListsApiTest.php

// If you require PHPUnit to be installed, first get PEAR:
//
// $ wget http://pear.php.net/go-pear.phar
// $ php -d detect_unicode=0 go-pear.phar
//
// Then install PHPUnit:
//
// $ pear config-set auto_discover
// $ pear install pear.phpunit.de/PHPUnit


require_once 'wordnik/Swagger.php';
// This used to be required, but now gives an error:
// Cannot redeclare phpunit_autoload()
// require_once '/usr/lib/php/PHPUnit/Autoload.php';

class BaseApiTest extends PHPUnit_Framework_TestCase {

  public function setUp() {
    $this->apiUrl = 'http://api.wordnik.com/v4';
    $this->apiKey = getenv('API_KEY');
    $this->username = getenv('USER_NAME');
    $this->password = getenv('PASSWORD');
    $this->client = new APIClient($this->apiKey, $this->apiUrl);
    $this->accountApi = new AccountApi($this->client);
    $this->wordApi = new WordApi($this->client);
    $this->wordListApi = new WordListApi($this->client);
    $this->wordListsApi = new WordListsApi($this->client);
    $this->wordsApi = new WordsApi($this->client);
  }

  public function tearDown() {
      unset($this->client);
  }

}


?>
