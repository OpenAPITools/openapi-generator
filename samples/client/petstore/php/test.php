<?php
//require_once('vendor/autoload.php');
require_once('SwaggerPetstore-php/SwaggerPetstore.php');

// initialize the API client
$api_client = new SwaggerPetstore\APIClient('http://petstore.swagger.io/v2');
$petId = 5;  // ID of pet that needs to be fetched
try {
    $pet_api = new SwaggerPetstore\PetAPI($api_client);
    // return Pet (model)
    $response = $pet_api->getPetById($petId);
    var_dump($response);
} catch (Exception $e) {
    echo 'Caught exception: ', $e->getMessage(), "\n";
}

?>
