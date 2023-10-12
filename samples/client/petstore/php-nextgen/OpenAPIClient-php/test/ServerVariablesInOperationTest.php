<?php

namespace OpenAPI\Client;

use InvalidArgumentException;
use OpenAPI\Client\Api\PetApi;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/FakeHttpClient.php';

/**
 * Tests for server variables in operations
 */
class ServerVariablesInOperationTest extends TestCase
{
    private FakeHttpClient $fakeHttpClient;
    private PetApi $api;
    private Model\Pet $pet;

    public function setUp(): void
    {
        $this->fakeHttpClient = new FakeHttpClient();
        $this->api = new Api\PetApi($this->fakeHttpClient);
        $this->pet = new Model\Pet();
        $this->pet->setName("something");
        $this->pet->setPhotoUrls(array("https://a.com"));
    }

    public function testServerVariablesInOperation(): void
    {
        # Test default values (if no variables are set)
        $this->api->addPet($this->pet, 2);
        $request = $this->fakeHttpClient->getLastRequest();

        $this->assertEquals('petstore.swagger.io', $request->getUri()->getHost(), 'Server variable set to default value.');

        # Test variables substitution
        $this->api->addPet($this->pet, 2, [ 'server' => 'qa-petstore', 'port' => '8080']);
        $request = $this->fakeHttpClient->getLastRequest();

        $this->assertEquals('qa-petstore.swagger.io', $request->getUri()->getHost(), 'Server set to "qa-petstore"');
        $this->assertEquals(8080, $request->getUri()->getPort(), 'Port set to 8080');
    }

    public function testLegacyServerChoiceInOperation(): void
    {
        # Test legacy behavior (set server using api->setHostIndex()
        $this->api->setHostIndex(1);
        $this->api->addPet($this->pet);
        $request = $this->fakeHttpClient->getLastRequest();

        $this->assertEquals('path-server-test.petstore.local', $request->getUri()->getHost(), 'Server set using legacy behavior');
    }

    public function testInvalidVariableValueInOperation(): void
    {
        $this->expectException(InvalidArgumentException::class);
        $this->api->addPet($this->pet, 2,['server' => 'invalid-value']);
    }
}
