<?php

require_once('autoload.php');

// increase memory limit to avoid fatal error due to findPetByStatus
// returning a lot of data
ini_set('memory_limit', '256M');

class PetApiTest extends \PHPUnit_Framework_TestCase
{

    // add a new pet (id 10005) to ensure the pet object is available for all the tests
    public static function setUpBeforeClass() {
        // for error reporting (need to run with php5.3 to get no warning)
        //ini_set('display_errors', 1);
        //error_reporting(~0);
        // when running with php5.5, comment out below to skip the warning about
        // using @ to handle file upload
        //ini_set('display_startup_errors',1);
        //ini_set('display_errors',1);
        //error_reporting(-1);
  
        // enable debugging 
        //Swagger\Client\Configuration::$debug = true;
  
        // skip initializing the API client as it should be automatic
        //$api_client = new Swagger\Client\ApiClient('http://petstore.swagger.io/v2');
        // new pet
        $new_pet_id = 10005;
        $new_pet = new Swagger\Client\Model\Pet;
        $new_pet->setId($new_pet_id);
        $new_pet->setName("PHP Unit Test");
        $new_pet->setPhotoUrls(array("http://test_php_unit_test.com"));
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
  
        $pet_api = new Swagger\Client\Api\PetApi();
        // add a new pet (model)
        $add_response = $pet_api->addPet($new_pet);
    }
  
    // test static functions defined in ApiClient
    public function testApiClient()
    { 
        // test selectHeaderAccept
        $api_client = new Swagger\Client\ApiClient();
        $this->assertSame('application/json', $api_client->selectHeaderAccept(array('application/xml','application/json')));
        $this->assertSame(NULL, $api_client->selectHeaderAccept(array()));
        $this->assertSame('application/yaml,application/xml', $api_client->selectHeaderAccept(array('application/yaml','application/xml')));
       
        // test selectHeaderContentType
        $this->assertSame('application/json', $api_client->selectHeaderContentType(array('application/xml','application/json')));
        $this->assertSame('application/json', $api_client->selectHeaderContentType(array()));
        $this->assertSame('application/yaml,application/xml', $api_client->selectHeaderContentType(array('application/yaml','application/xml')));
  
        // test addDefaultHeader and getDefaultHeader
        $api_client->getConfig()->addDefaultHeader('test1', 'value1');
        $api_client->getConfig()->addDefaultHeader('test2', 200);
        $defaultHeader = $api_client->getConfig()->getDefaultHeaders();
        $this->assertSame('value1', $defaultHeader['test1']);
        $this->assertSame(200, $defaultHeader['test2']);
  
        // test deleteDefaultHeader
        $api_client->getConfig()->deleteDefaultHeader('test2');
        $defaultHeader = $api_client->getConfig()->getDefaultHeaders();
        $this->assertFalse(isset($defaultHeader['test2']));
  
        $pet_api2 = new Swagger\Client\Api\PetApi();
        $config3 = new Swagger\Client\Configuration();
        $apiClient3 = new Swagger\Client\ApiClient($config3);
        $apiClient3->getConfig()->setUserAgent('api client 3');
        $config4 = new Swagger\Client\Configuration();
        $apiClient4 = new Swagger\Client\ApiClient($config4);
        $apiClient4->getConfig()->setUserAgent('api client 4');
        $pet_api3 = new Swagger\Client\Api\PetApi($apiClient3);
  
        // 2 different api clients are not the same 
        $this->assertNotEquals($apiClient3, $apiClient4);
        // customied pet api not using the old pet api's api client
        $this->assertNotEquals($pet_api2->getApiClient(), $pet_api3->getApiClient());
  
        // test access token
        $api_client->getConfig()->setAccessToken("testing_only");
        $this->assertSame('testing_only', $api_client->getConfig()->getAccessToken());
    }
  
    // test getPetById with a Pet object (id 10005)
    public function testGetPetById()
    {
        // initialize the API client without host
        $pet_id = 10005;  // ID of pet that needs to be fetched
        $pet_api = new Swagger\Client\Api\PetApi();
        $pet_api->getApiClient()->getConfig()->setApiKey('api_key', '111222333444555');
        // return Pet (model)
        $response = $pet_api->getPetById($pet_id);
        $this->assertSame($response->getId(), $pet_id);
        $this->assertSame($response->getName(), 'PHP Unit Test');
        $this->assertSame($response->getPhotoUrls()[0], 'http://test_php_unit_test.com');
        $this->assertSame($response->getCategory()->getId(), $pet_id);
        $this->assertSame($response->getCategory()->getName(), 'test php category');
        $this->assertSame($response->getTags()[0]->getId(), $pet_id);
        $this->assertSame($response->getTags()[0]->getName(), 'test php tag');
    }

    /* 
     * comment out as we've removed invalid endpoints from the spec, we'll introduce something
     * similar in the future when we've time to update the petstore server
     *
    // test getPetById with a Pet object (id 10005)
    public function testGetPetByIdInObject()
    {
        // initialize the API client without host
        $pet_id = 10005;  // ID of pet that needs to be fetched
        $pet_api = new Swagger\Client\Api\PetApi();
        $pet_api->getApiClient()->getConfig()->setApiKey('api_key', '111222333444555');
        // return Pet (inline model)
        $response = $pet_api->getPetByIdInObject($pet_id);
        $this->assertInstanceOf('Swagger\Client\Model\InlineResponse200', $response);
        $this->assertSame($response->getId(), $pet_id);
        $this->assertSame($response->getName(), 'PHP Unit Test');
        $this->assertSame($response->getPhotoUrls()[0], 'http://test_php_unit_test.com');

        // category is type "object"
        $this->assertInternalType('array', $response->getCategory());
        $this->assertSame($response->getCategory()['id'], $pet_id);
        $this->assertSame($response->getCategory()['name'], 'test php category');

        $this->assertSame($response->getTags()[0]->getId(), $pet_id);
        $this->assertSame($response->getTags()[0]->getName(), 'test php tag');
    }
     */
  
    // test getPetByIdWithHttpInfo with a Pet object (id 10005)
    public function testGetPetByIdWithHttpInfo()
    {
        // initialize the API client without host
        $pet_id = 10005;  // ID of pet that needs to be fetched
        $pet_api = new Swagger\Client\Api\PetApi();
        $pet_api->getApiClient()->getConfig()->setApiKey('api_key', '111222333444555');
        // return Pet (model)
        list($response, $status_code, $response_headers) = $pet_api->getPetByIdWithHttpInfo($pet_id);
        $this->assertSame($response->getId(), $pet_id);
        $this->assertSame($response->getName(), 'PHP Unit Test');
        $this->assertSame($response->getCategory()->getId(), $pet_id);
        $this->assertSame($response->getCategory()->getName(), 'test php category');
        $this->assertSame($response->getTags()[0]->getId(), $pet_id);
        $this->assertSame($response->getTags()[0]->getName(), 'test php tag');
        $this->assertSame($status_code, 200);
        $this->assertSame($response_headers['Content-Type'], 'application/json');
    }
  
    // test getPetByStatus and verify by the "id" of the response
    public function testFindPetByStatus()
    {
        // initialize the API client
        $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\ApiClient($config);
        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // return Pet (model)
        $response = $pet_api->findPetsByStatus("available");
        $this->assertGreaterThan(0, count($response)); // at least one object returned
        $this->assertSame(get_class($response[0]), "Swagger\\Client\\Model\\Pet"); // verify the object is Pet
        // loop through result to ensure status is "available" 
        foreach ($response as $_pet) {
            $this->assertSame($_pet['status'], "available");
        }
        // test invalid status 
        $response = $pet_api->findPetsByStatus("unknown_and_incorrect_status");
        $this->assertSame(count($response), 0); // confirm no object returned
    }
  
    // test getPetsByTags and verify by the "id" of the response
    public function testFindPetsByTags()
    {
        // initialize the API client
        $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\ApiClient($config);
        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // return Pet (model)
        $response = $pet_api->findPetsByTags("test php tag");
        $this->assertGreaterThan(0, count($response)); // at least one object returned
        $this->assertSame(get_class($response[0]), "Swagger\\Client\\Model\\Pet"); // verify the object is Pet
        // loop through result to ensure status is "available" 
        foreach ($response as $_pet) {
            $this->assertSame($_pet['tags'][0]['name'], "test php tag");
        }
        // test invalid status 
        $response = $pet_api->findPetsByTags("unknown_and_incorrect_tag");
        $this->assertSame(count($response), 0); // confirm no object returned
    }

    // test updatePet (model/json)and verify by the "id" of the response
    public function testUpdatePet()
    {
        // initialize the API client
        $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\ApiClient($config);
        $pet_id = 10001;  // ID of pet that needs to be fetched
        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // create updated pet object
        $updated_pet = new Swagger\Client\Model\Pet;
        $updated_pet->setId($pet_id);
        $updated_pet->setName('updatePet'); // new name
        $updated_pet->setStatus('pending'); // new status
        // update Pet (model/json)
        $update_response = $pet_api->updatePet($updated_pet);
        // return nothing (void)
        $this->assertSame($update_response, NULL);
        // verify updated Pet
        $response = $pet_api->getPetById($pet_id);
        $this->assertSame($response->getId(), $pet_id);
        $this->assertSame($response->getStatus(), 'pending');
        $this->assertSame($response->getName(), 'updatePet');
    }
  
    // test updatePetWithFormWithHttpInfo and verify by the "name" of the response
    public function testUpdatePetWithFormWithHttpInfo()
    {
        // initialize the API client
        $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\ApiClient($config);
        $pet_id = 10001;  // ID of pet that needs to be fetched
        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // update Pet (form)
        list($update_response, $status_code, $http_headers) = $pet_api->updatePetWithFormWithHttpInfo($pet_id, 'update pet with form with http info');
        // return nothing (void)
        $this->assertNull($update_response);
        $this->assertSame($status_code, 200);
        $this->assertSame($http_headers['Content-Type'], 'application/json');
        $response = $pet_api->getPetById($pet_id);
        $this->assertSame($response->getId(), $pet_id);
        $this->assertSame($response->getName(), 'update pet with form with http info');
    }
  
    // test updatePetWithForm and verify by the "name" and "status" of the response
    public function testUpdatePetWithForm()
    {
        // initialize the API client
        $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\ApiClient($config);
        $pet_id = 10001;  // ID of pet that needs to be fetched
        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // update Pet (form)
        $update_response = $pet_api->updatePetWithForm($pet_id, 'update pet with form', 'sold');
        // return nothing (void)
        $this->assertSame($update_response, NULL);
        $response = $pet_api->getPetById($pet_id);
        $this->assertSame($response->getId(), $pet_id);
        $this->assertSame($response->getName(), 'update pet with form');
        $this->assertSame($response->getStatus(), 'sold');
    }

    // test addPet and verify by the "id" and "name" of the response
    public function testAddPet()
    {
        // initialize the API client
        $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\ApiClient($config);
        $new_pet_id = 10005;
        $new_pet = new Swagger\Client\Model\Pet;
        $new_pet->setId($new_pet_id);
        $new_pet->setName("PHP Unit Test 2");
        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // add a new pet (model)
        $add_response = $pet_api->addPet($new_pet);
        // return nothing (void)
        $this->assertSame($add_response, NULL);
        // verify added Pet
        $response = $pet_api->getPetById($new_pet_id);
        $this->assertSame($response->getId(), $new_pet_id);
        $this->assertSame($response->getName(), 'PHP Unit Test 2');
    }

    /*
     * comment out as we've removed invalid endpoints from the spec, we'll introduce something
     * similar in the future when we've time to update the petstore server
     *
    // test addPetUsingByteArray and verify by the "id" and "name" of the response
    public function testAddPetUsingByteArray()
    {
        // initialize the API client
        $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\ApiClient($config);

        $new_pet_id = 10005;
        $new_pet = new Swagger\Client\Model\Pet;
        $new_pet->setId($new_pet_id);
        $new_pet->setName("PHP Unit Test 3");
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

        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // add a new pet (model)
        $object_serializer = new Swagger\Client\ObjectSerializer();
        $pet_json_string = json_encode($object_serializer->sanitizeForSerialization($new_pet));
        $add_response = $pet_api->addPetUsingByteArray($pet_json_string);
        // return nothing (void)
        $this->assertSame($add_response, NULL);
        // verify added Pet
        $response = $pet_api->getPetById($new_pet_id);
        $this->assertSame($response->getId(), $new_pet_id);
        $this->assertSame($response->getName(), 'PHP Unit Test 3');
    }
     */
  
    // test upload file
    public function testUploadFile()
    {
        // initialize the API client
        $config = (new Swagger\Client\Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\ApiClient($config);
        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // upload file
        $pet_id = 10001;
        $response = $pet_api->uploadFile($pet_id, "test meta", "./composer.json");
        // return ApiResponse 
        $this->assertInstanceOf('Swagger\Client\Model\ApiResponse', $response);

    }
  
    // test get inventory
    public function testGetInventory()
    {
        // initialize the API client
        $config = new Swagger\Client\Configuration();
        $config->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\APIClient($config);
        $store_api = new Swagger\Client\Api\StoreApi($api_client);
        // get inventory
        $get_response = $store_api->getInventory();
  
        $this->assertInternalType("int", $get_response['sold']);
        $this->assertInternalType("int", $get_response['pending']);
    }

    /*
     * comment out as we've removed invalid endpoints from the spec, we'll introduce something
     * similar in the future when we've time to update the petstore server
     *
    // test byte array response
    public function testGetPetByIdWithByteArray()
    {
        // initialize the API client
        $config = new Swagger\Client\Configuration();
        $config->setHost('http://petstore.swagger.io/v2');
        $api_client = new Swagger\Client\APIClient($config);
        $pet_api = new Swagger\Client\Api\PetApi($api_client);
        // test getPetByIdWithByteArray 
        $pet_id = 10005;
        $bytes = $pet_api->petPetIdtestingByteArraytrueGet($pet_id);
        $json = json_decode($bytes, true);

        $this->assertInternalType("string", $bytes);

        $this->assertSame($json['id'], $pet_id);
        // not testing name as it's tested by addPetUsingByteArray
        //$this->assertSame($json['name'], 'PHP Unit Test');
        $this->assertSame($json['category']['id'], $pet_id);
        $this->assertSame($json['category']['name'], 'test php category');
        $this->assertSame($json['tags'][0]['id'], $pet_id);
        $this->assertSame($json['tags'][0]['name'], 'test php tag');
    }
     */

    // test empty object serialization
    public function testEmptyPetSerialization()
    {
        $new_pet = new Swagger\Client\Model\Pet;
        // the empty object should be serialised to {}
        $this->assertSame("{}", "$new_pet");

    }

    // test inheritance in the model
    public function testInheritance()
    {
        $new_dog = new Swagger\Client\Model\Dog;
        // the object should be an instance of the derived class
        $this->assertInstanceOf('Swagger\Client\Model\Dog', $new_dog);
        // the object should also be an instance of the parent class
        $this->assertInstanceOf('Swagger\Client\Model\Animal', $new_dog);
    }

    // test inheritance constructor is working with data
    // initialization
    public function testInheritanceConstructorDataInitialization()
    {
        // initialize the object with data in the constructor
        $data = array(
            'class_name' => 'Dog',
            'breed' => 'Great Dane'
        );
        $new_dog = new Swagger\Client\Model\Dog($data);

        // the property on the derived class should be set
        $this->assertSame('Great Dane', $new_dog->getBreed());
        // the property on the parent class should be set
        $this->assertSame('Dog', $new_dog->getClassName());
    }

    // test if discriminator is initialized automatically
    public function testDiscriminatorInitialization()
    {
        $new_dog = new Swagger\Client\Model\Dog();
        $this->assertSame('Dog', $new_dog->getClassName());
    }

    // test if ArrayAccess interface works
    public function testArrayStuff()
    {
        // create an AnimalFarm which is an object implementing the
        // ArrayAccess interface
        $farm = new Swagger\Client\Model\AnimalFarm();

        // add some animals to the farm to make sure the ArrayAccess
        // interface works
        $farm[] = new Swagger\Client\Model\Dog();
        $farm[] = new Swagger\Client\Model\Cat();
        $farm[] = new Swagger\Client\Model\Animal();

        // assert we can look up the animals in the farm by array
        // indices (let's try a random order)
        $this->assertInstanceOf('Swagger\Client\Model\Cat', $farm[1]);
        $this->assertInstanceOf('Swagger\Client\Model\Dog', $farm[0]);
        $this->assertInstanceOf('Swagger\Client\Model\Animal', $farm[2]);

        // let's try to `foreach` the animals in the farm and let's
        // try to use the objects we loop through
        foreach ($farm as $animal) {
            $this->assertContains($animal->getClassName(), array('Dog', 'Cat', 'Animal'));
            $this->assertInstanceOf('Swagger\Client\Model\Animal', $animal);
        }
    }

}

?>

