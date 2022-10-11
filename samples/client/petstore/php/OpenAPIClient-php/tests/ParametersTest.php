<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Api\FakeApi;
use OpenAPI\Client\Api\UserApi;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/FakeHttpClient.php';

class ParametersTest extends TestCase
{
    /** @var  FakeHttpClient */
    private $fakeHttpClient;
    /** @var  FakeApi */
    private $fakeApi;
    /** @var  UserApi */
    private $userApi;

    public function setUp(): void
    {
        $this->fakeHttpClient = new FakeHttpClient();
        $this->fakeApi = new Api\FakeApi($this->fakeHttpClient);
        $this->userApi = new Api\UserApi($this->fakeHttpClient);
    }

    public function testHeaderParam()
    {
        $this->fakeApi->testEnumParameters([], 'something');

        $request = $this->fakeHttpClient->getLastRequest();
        $headers = $request->getHeaders();

        $this->assertArrayHasKey('enum_header_string', $headers);
        $this->assertEquals(['something'], $headers['enum_header_string']);
    }

    public function testHeaderParamCollection()
    {
        $this->fakeApi->testEnumParameters(['string1', 'string2']);

        $request = $this->fakeHttpClient->getLastRequest();
        $headers = $request->getHeaders();

        $this->assertArrayHasKey('enum_header_string_array', $headers);
        $this->assertEquals(['string1,string2'], $headers['enum_header_string_array']);
    }

    public function testInlineAdditionalProperties()
    {
        $param = new \stdClass();
        $param->foo = 'bar';
        $this->fakeApi->testInlineAdditionalProperties($param);

        $request = $this->fakeHttpClient->getLastRequest();
        $this->assertSame('{"foo":"bar"}', $request->getBody()->getContents());
    }

    /**
     * @see https://github.com/OpenAPITools/openapi-generator/pull/11225
     * @dataProvider provideQueryParameters
     */
    public function testQueryParameterCollectionFormatRequest($pipe, $ioutil, $http, $url, $context, $allow_empty, $language, $expected)
    {
        $request = $this->fakeApi->testQueryParameterCollectionFormatRequest($pipe, $ioutil, $http, $url, $context, $allow_empty, $language);
        $this->assertEquals($expected, urldecode($request->getUri()->getQuery()));
    }

    public function provideQueryParameters()
    {
        $array = ['blue', 'black', 'brown'];
        $object = ['R' => 100, 'G' => 200, 'B' => 150];
        return [
            [
                $array, $array, $array, $array, $array, 'blue', $object, 'pipe=blue|black|brown&ioutil=blue,black,brown&http=blue black brown&url=blue,black,brown&context=blue&context=black&context=brown&R=100&G=200&B=150&allowEmpty=blue',
            ],
        ];
    }

//    missing example for collection path param in config
//    public function testPathParamCollection()
//    {
//        $this->userApi->getUserByNameWithHttpInfo(['aa', 'bb']);
//        $request = $this->fakeHttpClient->getLastRequest();
//        $this->assertEquals('user/aa,bb', urldecode($request->getUri()->getPath()));
//    }
}
