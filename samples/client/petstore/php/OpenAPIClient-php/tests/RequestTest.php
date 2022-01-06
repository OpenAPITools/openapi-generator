<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Api\FakeApi;
use OpenAPI\Client\Model\User;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/FakeHttpClient.php';

class RequestTest extends TestCase
{
    /** @var FakeApi */
    private $api;
    /** @var  FakeHttpClient */
    private $fakeClient;

    public function setUp(): void
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

    public function testDeepObjectParametersInQueryString()
    {
        $queryparam = $this->api->testDeepObjectQueryParams(['search' => ['like' => 'test']]);
        $this->assertSame('filter%5Bsearch%5D%5Blike%5D=test', $queryparam);
        // for better readability
        $this->assertSame('filter[search][like]=test', urldecode($queryparam));

        // if nothing is provided
        $this->assertSame('', $this->api->testDeepObjectQueryParams(null));

        // if nothing is provided
        $this->assertSame('filter=bar', $this->api->testDeepObjectQueryParams('bar'));
        $this->assertSame('filter=bar', urldecode($this->api->testDeepObjectQueryParams('bar')));

        // do the same tests with the old behavior: testDeepObjectQueryParamsOldBehavior

        // if nothing is provided
        $this->assertSame('', $this->api->testDeepObjectQueryParamsOldBehavior(null));

        // if nothing is provided
        $this->assertSame('filter=bar', $this->api->testDeepObjectQueryParamsOldBehavior('bar'));
        $this->assertSame('filter=bar', urldecode($this->api->testDeepObjectQueryParamsOldBehavior('bar')));
    }
}
