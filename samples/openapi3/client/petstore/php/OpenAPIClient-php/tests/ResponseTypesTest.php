<?php

namespace OpenAPI\Client;

use GuzzleHttp\Psr7\Response;
use OpenAPI\Client\Api\PetApi;
use OpenAPI\Client\Model\Pet;
use PHPUnit\Framework\TestCase;

require_once __DIR__.'/FakeHttpClient.php';

class ResponseTypesTest extends TestCase
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

    /** @var PetApi */
    private $api;
    /** @var FakeHttpClient */
    private $fakeHttpClient;

    public function setUp()
    {
        $this->fakeHttpClient = new FakeHttpClient();
        $this->api = new PetApi($this->fakeHttpClient);
    }

    public function testDefined200ReturnType()
    {
        // given
        $statusCode = 200;
        $this->fakeHttpClient->setResponse(
            new Response($statusCode, [], json_encode(self::SAMPLE_PET))
        );

        // when
        $result = $this->api->getPetById(123);

        // then
        $this->assertInstanceOf(Pet::class, $result);
    }

    public function testDefault2xxReturnType()
    {
        // given
        $statusCode = 255;
        $this->fakeHttpClient->setResponse(
            new Response($statusCode, [], json_encode(self::SAMPLE_PET))
        );

        // when
        $result = $this->api->getPetById(123);

        // then
        $this->assertInstanceOf(Pet::class, $result);
    }

    /**
     * @expectedException \OpenAPI\Client\ApiException
     * @expectedExceptionCode 400
     */
    public function testDefinedErrorException()
    {
        $statusCode = 400;

        $this->fakeHttpClient->setResponse(new Response($statusCode, [], '{}'));
        $this->api->getPetById(123);
    }

//    missing case in spec:
//    responses:
//        '400':
//        description: failure
//        schema:
//            $ref: '#/definitions/Error'
//    public function testDefinedErrorResponseObject()
//    {
//        $result = null;
//        try {
//            $this->fakeHttpClient->setResponse(new Response(400, [], '{}'));
//            $this->api->getPetById(123);
//        } catch (ApiException $e) {
//            $result = $e->getResponseObject();
//        }
//
//        $this->assertInstanceOf(Error::class, $result);
//    }

    /**
     * @expectedException \OpenAPI\Client\ApiException
     * @expectedExceptionCode 404
     */
    public function testDefaultErrorException()
    {
        $statusCode = 404;

        $this->fakeHttpClient->setResponse(new Response($statusCode, [], '{}'));
        $this->api->getPetById(123);
    }

    public function testDefaultErrorResponseObject()
    {
        $result = null;
        try {
            $this->fakeHttpClient->setResponse(new Response(404, [], '{}'));
            $this->api->getPetById(123);
        } catch (ApiException $e) {
            $result = $e->getResponseObject();
        }

        $this->assertNull($result);
    }
}
