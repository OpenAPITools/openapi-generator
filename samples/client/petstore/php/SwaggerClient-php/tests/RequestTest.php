<?php

namespace Swagger\Client;

use Swagger\Client\Api\FakeApi;

class RequestTest extends \PHPUnit_Framework_TestCase
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
        $this->assertEquals(['application/json'], $contentType);

        $requestContent = $request->getBody()->getContents();

        $expected = json_encode(['param' => 'value', 'param2' => 'value2']);
        $this->assertEquals($expected, $requestContent);
    }
}
