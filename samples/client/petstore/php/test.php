<?php
//require_once('vendor/autoload.php');
require_once('SwaggerClient-php/SwaggerClient.php');

// initialize the API client
//$api_client = new SwaggerClient\ApiClient('http://petstore.swagger.io/v2');
//$api_client->addDefaultHeader("test1", "value1");

// to enable logging
//SwaggerClient\Configuration::$debug = true;
//SwaggerClient\Configuration::$debug_file = '/var/tmp/php_debug.log';

$petId = 10005; // ID of pet that needs to be fetched
try {
    //$pet_api = new SwaggerClient\PetAPI($api_client);
    $pet_api = new SwaggerClient\PetAPI();
    // return Pet (model)
    $response = $pet_api->getPetById($petId);
    var_dump($response);

    // test upload file (exception)
    $upload_response = $pet_api->uploadFile($petId, "test meta", NULL);

} catch (SwaggerClient\ApiException $e) {
    echo 'Caught exception: ', $e->getMessage(), "\n";
    echo 'HTTP response headers: ', $e->getResponseHeaders(), "\n";
    echo 'HTTP response body: ', $e->getResponseBody(), "\n";
    echo 'HTTP status code: ', $e->getCode(), "\n";
}

?>
