<?php
require_once(__DIR__ . '/SwaggerClient-php/vendor/autoload.php');

// show error reporting
//ini_set('display_errors', 1);
//error_reporting(~0);

// to debug report
print Swagger\Client\Configuration::toDebugReport();

// to change temp folder path
Swagger\Client\Configuration::getDefaultConfiguration()->setTempFolderPath('/var/tmp/php/');
// to enable logging
Swagger\Client\Configuration::getDefaultConfiguration()->setDebug(true);
Swagger\Client\Configuration::getDefaultConfiguration()->setDebugFile('/var/tmp/php_debug.log');

$petId = 10005; // ID of pet that needs to be fetched
try {
    // get pet by id
    //$api_client = new Swagger\Client\ApiClient('http://petstore.swagger.io/v2');
    //$api_client->getConfig()->addDefaultHeader("test1", "value1");
    //$pet_api = new Swagger\Client\PetAPI($api_client);
    $config = new \Swagger\Client\Configuration();
    $petApi = new Swagger\Client\Api\PetApi(null, $config);
    $config->setTempFolderPath('/var/tmp/php/');
    // test default header
    //$pet_api->getApiClient()->addDefaultHeader("TEST_API_KEY", "09182sdkanafndsl903");
    // return Pet (model)
    $response = $petApi->getPetById($petId);
    // to test __toString()
    print ($response);

    // add pet (post json)
    $newPetId = 10005;
    $newPet = new Swagger\Client\Model\Pet;
    $newPet->setId($newPetId);
    $newPet->setName("PHP Unit Test");
    // new tag
    $tag = new Swagger\Client\Model\Tag;
    $tag->setId($newPetId); // use the same id as pet
    //$tag->name = "test php tag";
    // new category
    $category = new Swagger\Client\Model\Category;
    $category->setId(10005); // use the same id as pet
    //$category->name = "test php category";

    $newPet->setTags(array($tag));
    $newPet->setCategory($category);

    $petApi = new Swagger\Client\Api\PetApi(null, $config);
    // add a new pet (model)
    $add_response = $petApi->addPet($newPet);

    // test upload file (should return exception)
    $upload_response = $petApi->uploadFile($petId, "test meta", NULL);

} catch (Swagger\Client\ApiException $e) {
    echo 'Caught exception: ', $e->getMessage(), "\n";
    echo 'HTTP response headers: ', print_r($e->getResponseHeaders(), true), "\n";
    echo 'HTTP response body: ', print_r($e->getResponseBody(), true), "\n";
    echo 'HTTP status code: ', $e->getCode(), "\n";
}
