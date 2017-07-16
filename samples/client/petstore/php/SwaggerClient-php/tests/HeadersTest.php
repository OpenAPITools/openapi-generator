<?php

namespace Swagger\Client;

require_once __DIR__ . '/FakeHttpClient.php';

class HeadersTest extends \PHPUnit_Framework_TestCase
{
    /** @var  FakeHttpClient */
    private $fakeHttpClient;

    public function setUp()
    {
        $this->fakeHttpClient = new FakeHttpClient();
    }

    public function testUserAgent()
    {
        $config = new Configuration();
        $config->setUserAgent('value');
        $api = new Api\PetApi($this->fakeHttpClient, $config);

        $api->getPetById(3);

        $request = $this->fakeHttpClient->getLastRequest();
        $headers = $request->getHeaders();

        $this->assertArrayHasKey('User-Agent', $headers);
        $this->assertEquals(['value'], $headers['User-Agent']);
    }
}
