<?php

namespace OpenAPI\Client;

use GuzzleHttp\Client;
use PHPUnit\Framework\TestCase;

class ExceptionTest extends TestCase
{
    public function testNotFound()
    {
        $this->expectException(\OpenAPI\Client\ApiException::class);
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
        $this->expectException(\OpenAPI\Client\ApiException::class);
        $this->expectExceptionMessage('Could not resolve');
        $this->expectExceptionMessage('wrong_host.zxc');
        $config = new Configuration();
        $config->setHost('http://wrong_host.zxc');

        $api = new Api\StoreApi(
            new Client(),
            $config
        );
        $api->getInventory();
    }
}
