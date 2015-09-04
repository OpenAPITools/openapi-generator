<?php

require_once('autoload.php');

class StoreApiTest extends \PHPUnit_Framework_TestCase
{

  // add a new pet (id 10005) to ensure the pet object is available for all the tests
  public static function setUpBeforeClass() {
    // for error reporting (need to run with php5.3 to get no warning)
    //ini_set('display_errors', 1);
    //error_reporting(~0);
    // new pet
    $new_pet_id = 10005;
    $new_pet = new Swagger\Client\Model\Pet;
    $new_pet->setId($new_pet_id);
    $new_pet->setName("PHP Unit Test");
    $new_pet->setStatus("available");
    // new tag
    $tag= new Swagger\Client\Model\Tag;
    $tag->setId($new_pet_id); // use the same id as pet
    $tag->setName("test php tag");
    // new category
    $category = new Swagger\Client\Model\Category;
    $category->setId($new_pet_id); // use the same id as pet
    $category->setName("test php category");

    $new_pet->setTags(array($tag));
    $new_pet->setCategory($category);

    $pet_api = new Swagger\Client\Api\PetAPI();
    // add a new pet (model)
    $add_response = $pet_api->addPet($new_pet);
  }

  // test get inventory
  public function testGetInventory()
  {
    // initialize the API client
    $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
    $api_client = new Swagger\Client\ApiClient($config);
    $store_api = new Swagger\Client\Api\StoreAPI($api_client);
    // get inventory
    $get_response = $store_api->getInventory();

    $this->assertInternalType("int", $get_response['available']);

  }

}

?>

