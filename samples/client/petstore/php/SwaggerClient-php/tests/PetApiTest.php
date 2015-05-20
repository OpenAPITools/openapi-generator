<?php

require_once('SwaggerClient.php');

class PetApiTest extends \PHPUnit_Framework_TestCase
{

  // add a new pet (id 10005) to ensure the pet object is available for all the tests
  public static function setUpBeforeClass() {
    // initialize the API client
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
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

    $new_pet->tags = [$tag];
    $new_pet->category = $category;

    $pet_api = new SwaggerClient\PetAPI($api_client);
    // add a new pet (model)
    $add_response = $pet_api->addPet($new_pet);
  }

  public function testConfiguration() 
  {
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
    SwaggerClient\Configuration::$apiKey['api_key'] = '123456';
    $headerParams = array('test1' => 'value1');
    $queryParams = array('test2' => 'value2');
    $authSettings = array('api_key', 'unknown');
    
    # test prefix
    SwaggerClient\Configuration::$apiKeyPrefix['api_key'] = 'PREFIX';
    $this->assertSame('PREFIX', SwaggerClient\Configuration::$apiKeyPrefix['api_key']);

    # update parameters based on auth setting
    $api_client->updateParamsForAuth($headerParams, $queryParams, $authSettings);

    # test api key 
    $this->assertSame($headerParams['test1'], 'value1');
    $this->assertSame($headerParams['api_key'], 'PREFIX 123456');
    $this->assertSame($queryParams['test2'], 'value2');

    # test http basic auth
    SwaggerClient\Configuration::$username = 'test_username';
    SwaggerClient\Configuration::$password = 'test_password';
    $this->assertSame('test_username', SwaggerClient\Configuration::$username);
    $this->assertSame('test_password', SwaggerClient\Configuration::$password);



  }

  // test getPetById with a Pet object (id 10005)
  public function testGetPetById()
  {
    // initialize the API client
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
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
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
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
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
    $pet_id = 10001;  // ID of pet that needs to be fetched
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // create updated pet object
    $updated_pet = new SwaggerClient\models\Pet;
    $updated_pet->id = $pet_id;
    $updated_pet->status = "pending"; // new status
    // update Pet (model/json)
    $update_response = $pet_api->updatePet($updated_pet);
    // return nothing (void)
    $this->assertSame($update_response, NULL);
    // verify updated Pet
    $response = $pet_api->getPetById($pet_id);
    $this->assertSame($response->id, $pet_id);
    $this->assertSame($response->status, 'pending');
  }

  // test updatePet and verify by the "id" of the response
  public function testUpdatePetWithForm()
  {
    // initialize the API client
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
    $pet_id = 10001;  // ID of pet that needs to be fetched
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // update Pet (form)
    $update_response = $pet_api->updatePetWithForm($pet_id, null, 'sold');
    // return nothing (void)
    $this->assertSame($update_response, NULL);
    // TODO commented out for the time being since it's broken
    // https://github.com/swagger-api/swagger-codegen/issues/656
    // verify updated Pet
    //$response = $pet_api->getPetById($pet_id);
    //$this->assertSame($response->id, $pet_id);
    //$this->assertSame($response->status, 'sold');
  }

  // test addPet and verify by the "id" and "name" of the response
  public function testAddPet()
  {
    // initialize the API client
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
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

  // test 
  public function testUploadFile()
  {
    // initialize the API client
    $api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // upload file
    $pet_id = 10001;
    $add_response = $pet_api->uploadFile($pet_id, "test meta", "./composer.json");
    // return nothing (void)
    $this->assertSame($add_response, NULL);
  }

}

?>

