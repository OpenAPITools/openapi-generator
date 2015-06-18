<?php

require_once('SwaggerClient.php');

class StoreApiTest extends \PHPUnit_Framework_TestCase
{

  // add a new pet (id 10005) to ensure the pet object is available for all the tests
  public static function setUpBeforeClass() {
    // for error reporting (need to run with php5.3 to get no warning)
    //ini_set('display_errors', 1);
    //error_reporting(~0);
  }

  // test get inventory
  public function testGetInventory()
  {
    // initialize the API client
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
    $store_api = new SwaggerClient\StoreAPI($api_client);
    // get inventory
    $get_response = $store_api->getInventory();

    $this->assertInternalType("int", $get_response['sold']);
    $this->assertInternalType("int", $get_response['pending']);

  }

}

?>

