<?php

namespace Swagger\Client;

use Swagger\Client\Api\FakeApi;
use Swagger\Client\Model\Body4;

require_once __DIR__ . '/FakeHttpClient.php';

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
        $this->api->testJsonFormData(
            new Body4(['param' => 'value', 'param2' => 'value2'])
        );

        $request = $this->fakeClient->getLastRequest();
        $contentType = $request->getHeader('Content-Type');
        $this->assertEquals(['application/json'], $contentType);

        $requestContent = $request->getBody()->getContents();
        $expected = <<<__EOS__
{
    "param": "value",
    "param2": "value2"
}
__EOS__;

        $this->assertEquals($expected, $requestContent);
    }
}
