<?php

require_once('SwaggerClient.php');

class PetApiTest extends \PHPUnit_Framework_TestCase
{

  // add a new pet (id 10005) to ensure the pet object is available for all the tests
  public static function setUpBeforeClass() {
    // for error reporting (need to run with php5.3 to get no warning)
    //ini_set('display_errors', 1);
    //error_reporting(~0);

    // enable debugging 
    //SwaggerClient\Configuration::$debug = true;

    // skip initializing the API client as it should be automatic
    //$api_client = new SwaggerClient\ApiClient('http://petstore.swagger.io/v2');
    // new pet
    $new_pet_id = 10005;
    $new_pet = new SwaggerClient\models\Pet;
    $new_pet->id = $new_pet_id;
    $new_pet->name = "PHP Unit Test";
    // new tag
    $tag= new SwaggerClient\models\Tag;
    $tag->id = $new_pet_id; // use the same id as pet
    $tag->name = "test php tag";
    // new category
    $category = new SwaggerClient\models\Category;
    $category->id = $new_pet_id; // use the same id as pet
    $category->name = "test php category";

    $new_pet->tags = array($tag);
    $new_pet->category = $category;

    $pet_api = new SwaggerClient\PetAPI();
    // add a new pet (model)
    $add_response = $pet_api->addPet($new_pet);
  }

  // test static functions defined in ApiClient
  public function testApiClient()
  { 
    // test selectHeaderAccept
    $api_client = new SwaggerClient\ApiClient();
    $this->assertSame('application/json', $api_client->selectHeaderAccept(array('application/xml','application/json')));
    $this->assertSame(NULL, $api_client->selectHeaderAccept(array()));
    $this->assertSame('application/yaml,application/xml', $api_client->selectHeaderAccept(array('application/yaml','application/xml')));
   
    // test selectHeaderContentType
    $this->assertSame('application/json', $api_client->selectHeaderContentType(array('application/xml','application/json')));
    $this->assertSame('application/json', $api_client->selectHeaderContentType(array()));
    $this->assertSame('application/yaml,application/xml', $api_client->selectHeaderContentType(array('application/yaml','application/xml')));

    // test addDefaultHeader and getDefaultHeader
    $api_client->addDefaultHeader('test1', 'value1');
    $api_client->addDefaultHeader('test2', 200);
    $defaultHeader = $api_client->getDefaultHeader();
    $this->assertSame('value1', $defaultHeader['test1']);
    $this->assertSame(200, $defaultHeader['test2']);

    // test deleteDefaultHeader
    $api_client->deleteDefaultHeader('test2');
    $defaultHeader = $api_client->getDefaultHeader();
    $this->assertFalse(isset($defaultHeader['test2']));

    $pet_api = new SwaggerClient\PetAPI();
    $pet_api2 = new SwaggerClient\PetAPI();
    $apiClient3 = new SwaggerClient\ApiClient();
    $apiClient3->setUserAgent = 'api client 3';
    $apiClient4 = new SwaggerClient\ApiClient();
    $apiClient4->setUserAgent = 'api client 4';
    $pet_api3 = new SwaggerClient\PetAPI($apiClient3);

    // same default api client
    $this->assertSame($pet_api->getApiClient(), $pet_api2->getApiClient());
    // confirm using the default api client in the Configuration
    $this->assertSame($pet_api->getApiClient(), SwaggerClient\Configuration::$apiClient);
    // 2 different api clients are not the same 
    $this->assertNotEquals($apiClient3, $apiClient4);
    // customized pet api not using the default (configuration) api client
    $this->assertNotEquals($pet_api3->getApiClient(), SwaggerClient\Configuration::$apiClient);
    // customied pet api not using the old pet api's api client
    $this->assertNotEquals($pet_api2->getApiClient(), $pet_api3->getApiClient());

    // both pet api and pet api2 share the same api client and confirm using timeout value
    $pet_api->getApiClient()->setTimeout(999);
    $this->assertSame(999, $pet_api2->getApiClient()->getTimeout());

  }

  // test getPetById with a Pet object (id 10005)
  public function testGetPetById()
  {
    // initialize the API client without host
    $api_client = new SwaggerClient\ApiClient();
    SwaggerClient\Configuration::$apiKey['api_key'] = '111222333444555';
    $pet_id = 10005;  // ID of pet that needs to be fetched
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // return Pet (model)
    $response = $pet_api->getPetById($pet_id);
    $this->assertSame($response->id, $pet_id);
    $this->assertSame($response->name, 'PHP Unit Test');
    $this->assertSame($response->category->id, $pet_id);
    $this->assertSame($response->category->name, 'test php category');
    $this->assertSame($response->tags[0]->id, $pet_id);
    $this->assertSame($response->tags[0]->name, 'test php tag');
  }

  // test getPetByStatus and verify by the "id" of the response
  public function testFindPetByStatus()
  {
    // initialize the API client
    $api_client = new SwaggerClient\ApiClient('http://petstore.swagger.io/v2');
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // return Pet (model)
    $response = $pet_api->findPetsByStatus("available");
    $this->assertGreaterThan(0, count($response)); // at least one object returned
    $this->assertSame(get_class($response[0]), "SwaggerClient\models\Pet"); // verify the object is Pet
    // loop through result to ensure status is "available" 
    foreach ($response as $_pet) {
      $this->assertSame($_pet['status'], "available");
    }
    // test invalid status 
    $response = $pet_api->findPetsByStatus("unknown_and_incorrect_status");
    $this->assertSame(count($response), 0); // confirm no object returned
  }

  // test updatePet (model/json)and verify by the "id" of the response
  public function testUpdatePet()
  {
    // initialize the API client
    $api_client = new SwaggerClient\ApiClient('http://petstore.swagger.io/v2');
    $pet_id = 10001;  // ID of pet that needs to be fetched
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // create updated pet object
    $updated_pet = new SwaggerClient\models\Pet;
    $updated_pet->id = $pet_id;
    $updated_pet->name = 'updatePet'; // new name
    $updated_pet->status = 'pending'; // new status
    // update Pet (model/json)
    $update_response = $pet_api->updatePet($updated_pet);
    // return nothing (void)
    $this->assertSame($update_response, NULL);
    // verify updated Pet
    $response = $pet_api->getPetById($pet_id);
    $this->assertSame($response->id, $pet_id);
    $this->assertSame($response->status, 'pending');
    $this->assertSame($response->name, 'updatePet');
  }

  // test updatePet and verify by the "id" of the response
  public function testUpdatePetWithForm()
  {
    // initialize the API client
    $api_client = new SwaggerClient\ApiClient('http://petstore.swagger.io/v2');
    $pet_id = 10001;  // ID of pet that needs to be fetched
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // update Pet (form)
    $update_response = $pet_api->updatePetWithForm($pet_id, 'update pet with form', 'sold');
    // return nothing (void)
    $this->assertSame($update_response, NULL);
    $response = $pet_api->getPetById($pet_id);
    $this->assertSame($response->id, $pet_id);
    $this->assertSame($response->name, 'update pet with form');
    $this->assertSame($response->status, 'sold');
  }

  // test addPet and verify by the "id" and "name" of the response
  public function testAddPet()
  {
    // initialize the API client
    $api_client = new SwaggerClient\ApiClient('http://petstore.swagger.io/v2');
    $new_pet_id = 10001;
    $new_pet = new SwaggerClient\models\Pet;
    $new_pet->id = $new_pet_id;
    $new_pet->name = "PHP Unit Test";
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // add a new pet (model)
    $add_response = $pet_api->addPet($new_pet);
    // return nothing (void)
    $this->assertSame($add_response, NULL);
    // verify added Pet
    $response = $pet_api->getPetById($new_pet_id);
    $this->assertSame($response->id, $new_pet_id);
    $this->assertSame($response->name, 'PHP Unit Test');
  }

  // test upload file
  public function testUploadFile()
  {
    // initialize the API client
    $api_client = new SwaggerClient\ApiClient('http://petstore.swagger.io/v2');
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // upload file
    $pet_id = 10001;
    $add_response = $pet_api->uploadFile($pet_id, "test meta", "./composer.json");
    // return nothing (void)
    $this->assertSame($add_response, NULL);
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

