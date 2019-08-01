<?php

namespace OpenAPI\Client;

use PHPUnit\Framework\TestCase;
use GuzzleHttp\Psr7\Response;

require_once __DIR__.'/FakeHttpClient.php';

class HeadersTest extends TestCase
{
    const SAMPLE_PET = [
        'id' => 1,
        'category' => [
            'id' => 1,
            'name' => 'any category name',
        ],
        'name' => 'any pet name',
        'photoUrls' => ['example.com'],
        'tags' => [
            ['id' => 1,
            'name' => 'any tag name',
            ],
        ],
        'status' => 'pending',
    ];

    /** @var FakeHttpClient */
    private $fakeHttpClient;

    public function setUp()
    {
        $this->fakeHttpClient = new FakeHttpClient();
        $this->fakeHttpClient->setResponse(
            new Response(200, [], json_encode(self::SAMPLE_PET))
        );
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
