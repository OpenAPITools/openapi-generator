<?php

namespace OpenAPI\Client;

use GuzzleHttp\Psr7\Response;
use OpenAPI\Client\Api\PetApi;
use OpenAPI\Client\Model\Pet;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/FakeHttpClient.php';

class ResponseTypesTest extends TestCase
{
    /** @var PetApi */
    private $api;
    /** @var  FakeHttpClient */
    private $fakeHttpClient;

    public function setUp(): void
    {
        $this->fakeHttpClient = new FakeHttpClient();
        $this->api = new PetApi($this->fakeHttpClient);
    }

    public function testDefined200ReturnType()
    {
        $this->fakeHttpClient->setResponse(new Response(200, [], json_encode([])));
        $result = $this->api->getPetById(123);

        $this->assertInstanceOf(Pet::class, $result);
    }

    public function testDefault2xxReturnType()
    {
        $this->fakeHttpClient->setResponse(new Response(255, [], json_encode([])));
        $result = $this->api->getPetById(123);

        $this->assertInstanceOf(Pet::class, $result);
    }

    public function testDefinedErrorException()
    {
        $this->expectExceptionCode(400);
        $this->expectException(\OpenAPI\Client\ApiException::class);
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

    public function testDefaultErrorException()
    {
        $this->expectExceptionCode(404);
        $this->expectException(\OpenAPI\Client\ApiException::class);
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

    public function invalidJSONResponseProvider()
    {
        return [
            'status 200, content empty' => [200, ''],
            'status 200, content leading comma' => [200, '{"key": "value",}'],
            'status 200, content just text' => [200, 'invalid JSON'],
            'status 200, content null' => [200, null],
            'status 204, content empty' => [204, ''],
            'status 204, content leading comma' => [204, '{"key": "value",}'],
            'status 204, content just text' => [204, 'invalid JSON'],
            'status 204, content null' => [204, null],
        ];
    }
    /**
     * @dataProvider invalidJSONResponseProvider
     */
    public function testNotJSONResponse($statusCode, $responseBody)
    {
        $this->expectExceptionCode($statusCode);
        $this->expectException(\OpenAPI\Client\ApiException::class);

        $this->fakeHttpClient->setResponse(new Response($statusCode, [], $responseBody));
        $this->api->getPetById(123);
    }
}
