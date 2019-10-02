<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Api\FakeApi;
use OpenAPI\Client\Api\PetApi;
use OpenAPI\Client\Model\Pet;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/FakeHttpClient.php';

class AuthTest extends TestCase
{
    public function testCustomApiKeyHeader()
    {
        $authConfig = new Configuration();
        $authConfig->setApiKey('api_key', '123qwe');

        $fakeHttpClient = new FakeHttpClient();
        $api = new PetApi($fakeHttpClient, $authConfig);
        $api->getPetById(123);

        $headers = $fakeHttpClient->getLastRequest()->getHeaders();

        $this->assertArrayHasKey('api_key', $headers);
        $this->assertEquals(['123qwe'], $headers['api_key']);
    }

    public function testApiToken()
    {
        $authConfig = new Configuration();
        $authConfig->setAccessToken('asd123');

        $fakeHttpClient = new FakeHttpClient();
        $api = new PetApi($fakeHttpClient, $authConfig);
        $api->addPet(new Pet());

        $headers = $fakeHttpClient->getLastRequest()->getHeaders();

        $this->assertArrayHasKey('Authorization', $headers);
        $this->assertEquals(['Bearer asd123'], $headers['Authorization']);
    }

    public function testBasicAuth()
    {
        $username = 'user';
        $password = 'password';

        $authConfig = new Configuration();
        $authConfig->setUsername($username);
        $authConfig->setPassword($password);

        $fakeHttpClient = new FakeHttpClient();
        $api = new FakeApi($fakeHttpClient, $authConfig);
        $api->testEndpointParameters(123, 100.1, 'ASD_', 'ASD');

        $headers = $fakeHttpClient->getLastRequest()->getHeaders();

        $this->assertArrayHasKey('Authorization', $headers);
        $encodedCredentials = base64_encode("$username:$password");
        $this->assertEquals(["Basic $encodedCredentials"], $headers['Authorization']);
    }
}
