<?php

require('SwaggerPetstore.php');

class PetApiTest extends \PHPUnit_Framework_TestCase
{
  public function testGetPetById()
  {
    // initialize the API client
    $api_client = new SwaggerPetstore\APIClient('http://petstore.swagger.io/v2');
    $petId = 5;  // ID of pet that needs to be fetched
    $pet_api = new SwaggerPetstore\PetAPI($api_client);
    // return Pet (model)
    $response = $pet_api->getPetById($petId);
    $this->assertSame($response->id, $petId);
  }
}

?>
