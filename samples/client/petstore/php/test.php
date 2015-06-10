<?php
//require_once('vendor/autoload.php');
require_once('SwaggerClient-php/SwaggerClient.php');

// show error reporting
ini_set('display_errors', 1);
error_reporting(~0);

// initialize the API client
//$api_client = new SwaggerClient\ApiClient('http://petstore.swagger.io/v2');
//$api_client->addDefaultHeader("test1", "value1");

// to enable logging
//SwaggerClient\Configuration::$debug = true;
//SwaggerClient\Configuration::$debug_file = '/var/tmp/php_debug.log';

$petId = 10005; // ID of pet that needs to be fetched
try {
    // get pet by id
    //$pet_api = new SwaggerClient\PetAPI($api_client);
    $pet_api = new SwaggerClient\PetAPI();
    // test default header
    $pet_api->getApiClient()->addDefaultHeader("TEST_API_KEY", "09182sdkanafndsl903");
    // return Pet (model)
    $response = $pet_api->getPetById($petId);
    var_dump($response);

    // test upload file (exception)
    $upload_response = $pet_api->uploadFile($petId, "test meta", NULL);

    // add pet (post json)
    $new_pet_id = 10005;
    $new_pet = new SwaggerClient\models\Pet;
    $new_pet->id = $new_pet_id;
    $new_pet->name = "PHP Unit Test";
    // new tag
    $tag= new SwaggerClient\models\Tag;
    $tag->id = $new_pet_id; // use the same id as pet
    //$tag->name = "test php tag";
    // new category
    $category = new SwaggerClient\models\Category;
    $category->id = 0; // use the same id as pet
    //$category->name = "test php category";

    $new_pet->tags = array($tag);
    $new_pet->category = $category;

    $pet_api = new SwaggerClient\PetAPI();
    // add a new pet (model)
    $add_response = $pet_api->addPet($new_pet);

} catch (Exception $e) {
    echo 'Caught exception: ', $e->getMessage(), "\n";
    echo 'HTTP response headers: ', $e->getResponseHeaders(), "\n";
    echo 'HTTP response body: ', $e->getResponseBody(), "\n";
    echo 'HTTP status code: ', $e->getCode(), "\n";
}


?>
