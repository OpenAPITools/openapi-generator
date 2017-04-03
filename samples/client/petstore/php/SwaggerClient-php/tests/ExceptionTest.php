<?php

namespace Swagger\Client;

use GuzzleHttp\Client;

class ExceptionTest extends \PHPUnit_Framework_TestCase
{
    public function testNotFound()
    {
        $this->expectException(ApiException::class);
        $this->expectExceptionCode(404);
        $this->expectExceptionMessage('http://petstore.swagger.io/INVALID_URL/store/inventory');

        $config = new Configuration();
        $config->setHost('http://petstore.swagger.io/INVALID_URL');

        $api = new Api\StoreApi(
            new Client(),
            $config
        );
        $api->getInventory();
    }

    public function testWrongHost()
    {
        $this->expectException(ApiException::class);
        $this->expectExceptionMessage('Could not resolve host');

        $config = new Configuration();
        $config->setHost('http://wrong_host.zxc');

        $api = new Api\StoreApi(
            new Client(),
            $config
        );
        $api->getInventory();
    }
}
