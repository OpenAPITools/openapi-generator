<?php
namespace OpenAPI\Client;

use PHPUnit\Framework\TestCase;

class DebugTest extends TestCase
{

    public static function setUpBeforeClass()
    {
        parent::setUpBeforeClass();
        $newPet = new Model\Pet;
        $newPet->setId(1);
        $newPet->setName("PHP Unit Test");
        (new Api\PetApi())->addPetWithHttpInfo($newPet);
    }

    public function testEnableDebugOutput()
    {
        $this->expectOutputRegex('#GET /v2/pet/1 HTTP/1.1#');

        $config = new Configuration();
        $config->setDebug(true);
        $api = new Api\PetApi(null, $config);
        $api->getPetById(1);
    }

    public function testEnableDebugOutputAsync()
    {
        $this->expectOutputRegex('#GET /v2/pet/1 HTTP/1.1#');

        $config = new Configuration();
        $config->setDebug(true);
        $api = new Api\PetApi(null, $config);
        $promise = $api->getPetByIdAsync(1);
        $promise->wait();
    }
}
