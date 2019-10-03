<?php

namespace OpenAPI\Client;

use GuzzleHttp\Client;
use PHPUnit\Framework\TestCase;

class ExceptionTest extends TestCase
{
    /**
     * @expectedException \OpenAPI\Client\ApiException
     * @expectedExceptionCode 404
     * @expectedExceptionMessage http://petstore.swagger.io/INVALID_URL/store/inventory
     */
    public function testNotFound()
    {
        $config = new Configuration();
        $config->setHost('http://petstore.swagger.io/INVALID_URL');

        $api = new Api\StoreApi(
            new Client(),
            $config
        );
        $api->getInventory();
    }

    /**
     * @expectedException \OpenAPI\Client\ApiException
     * @expectedExceptionMessage Could not resolve host
     */
    public function testWrongHost()
    {
        $config = new Configuration();
        $config->setHost('http://wrong_host.zxc');

        $api = new Api\StoreApi(
            new Client(),
            $config
        );
        $api->getInventory();
    }
}
