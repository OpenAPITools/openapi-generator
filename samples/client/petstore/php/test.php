<?php
//require_once('vendor/autoload.php');
require_once('SwaggerClient-php/SwaggerClient.php');

// initialize the API client
$api_client = new SwaggerClient\APIClient('http://petstore.swagger.io/v2');
$api_client->addDefaultHeader("test1", "value1");

$petId = 10005; // ID of pet that needs to be fetched
try {
    $pet_api = new SwaggerClient\PetAPI($api_client);
    // return Pet (model)
    $response = $pet_api->getPetById($petId);
    var_dump($response);
} catch (Exception $e) {
    echo 'Caught exception: ', $e->getMessage(), "\n";
}

?>
