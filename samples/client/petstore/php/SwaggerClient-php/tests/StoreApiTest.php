<?php

namespace Swagger\Client;

class StoreApiTest extends \PHPUnit_Framework_TestCase
{

    // add a new pet (id 10005) to ensure the pet object is available for all the tests
    public static function setUpBeforeClass()
    {
        // for error reporting (need to run with php5.3 to get no warning)
        //ini_set('display_errors', 1);
        //error_reporting(~0);
        // new pet
        $new_pet_id = 10005;
        $new_pet = new Model\Pet;
        $new_pet->setId($new_pet_id);
        $new_pet->setName("PHP Unit Test");
        $new_pet->setStatus("available");
        // new tag
        $tag= new Model\Tag;
        $tag->setId($new_pet_id); // use the same id as pet
        $tag->setName("test php tag");
        // new category
        $category = new Model\Category;
        $category->setId($new_pet_id); // use the same id as pet
        $category->setName("test php category");

        $new_pet->setTags(array($tag));
        $new_pet->setCategory($category);

        $pet_api = new Api\PetAPI();
        // add a new pet (model)
        $add_response = $pet_api->addPet($new_pet);
    }

    // test get inventory
    public function testGetInventory()
    {
        // initialize the API client
        $config = (new Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new ApiClient($config);
        $store_api = new Api\StoreApi($api_client);
        // get inventory
        $get_response = $store_api->getInventory();

        $this->assertInternalType("array", $get_response);
        $this->assertInternalType("int", $get_response['available']);
    }

    /*
     * comment out as we've removed invalid endpoints from the spec, we'll introduce something
     * similar in the future when we've time to update the petstore server
     *
    // test get inventory
    public function testGetInventoryInObject()
    {
        // initialize the API client
        //$config = (new Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new ApiClient();
        $store_api = new Api\StoreApi($api_client);
        // get inventory
        $get_response = $store_api->getInventoryInObject();

        $this->assertInternalType("array", $get_response);
        $this->assertInternalType("int", $get_response['available']);
    }
     */

    /**
     * test empty array response
     *
     * Make sure empty arrays from a producer is actually returned as
     * an empty array and not some other value. At some point it was
     * returned as null because the code stumbled on PHP loose type
     * checking (not on empty array is true, same thing could happen
     * with careless use of empty()).
     */
    public function testEmptyArrayResponse()
    {
        // initialize the API client
        $config = (new Configuration())->setHost('http://petstore.swagger.io/v2');
        $apiClient = new ApiClient($config);
        $storeApi = new Api\PetApi($apiClient);
        // this call returns and empty array
        $response = $storeApi->findPetsByStatus(array());

        // make sure this is an array as we want it to be
        $this->assertInternalType("array", $response);

        // make sure the array is empty just in case the petstore
        // server changes its output
        $this->assertEmpty($response);
    }
}
