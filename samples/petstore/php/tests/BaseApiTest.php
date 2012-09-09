<?php

// Unit tests for Swagger Petsore sample app PHP client

// Run all tests:
//
//     phpunit tests/PetApiTest.php
//     phpunit tests/StoreApiTest.php
//     phpunit tests/UserApiTest.php

// If you require PHPUnit to be installed, first get PEAR:
//
// $ wget http://pear.php.net/go-pear.phar
// $ php -d detect_unicode=0 go-pear.phar
//
// Then install PHPUnit:
//
// $ pear config-set auto_discover
// $ pear install pear.phpunit.de/PHPUnit

// If you want to see all the JSON, make sure you append the fake api-key:
// http://petstore.swagger.wordnik.com/api/pet.json?api_key=special-key

// You need to set this for your local time zone if not already set in
// global config.
date_default_timezone_set("America/Los_Angeles");

require_once 'petstore/Swagger.php';

class BaseApiTest extends PHPUnit_Framework_TestCase {

  public function setUp() {
    $this->apiKey = "special-key";
    $this->apiUrl = "http://petstore.swagger.wordnik.com/api";
    $this->username = "test";
    $this->password = "test";
    $this->client = new APIClient($this->apiKey, $this->apiUrl);
    $this->petApi = new PetApi($this->client);
    $this->storeApi = new StoreApi($this->client);
    $this->userApi = new UserApi($this->client);
  }

  public function tearDown() {
      unset($this->client);
  }

}


?>