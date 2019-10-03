<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Api\FakeApi;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/FakeHttpClient.php';

class RequestTest extends TestCase
{
    /** @var FakeApi */
    private $api;
    /** @var  FakeHttpClient */
    private $fakeClient;

    public function setUp()
    {
        $this->fakeClient = new FakeHttpClient();
        $this->api = new Api\FakeApi($this->fakeClient);
    }

    public function testFormDataEncodingToJson()
    {
        $this->api->testJsonFormData('value', 'value2');

        $request = $this->fakeClient->getLastRequest();
        $contentType = $request->getHeader('Content-Type');
        $this->assertEquals(['application/x-www-form-urlencoded'], $contentType);

        $requestContent = $request->getBody()->getContents();

        // JSON serialization of form data is not supported
        $this->assertEquals('param=value&param2=value2', $requestContent);
    }
}
