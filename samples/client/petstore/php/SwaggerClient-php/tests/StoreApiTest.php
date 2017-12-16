<?php

namespace Swagger\Client;

use Swagger\Client\Api\StoreApi;

class StoreApiTest extends \PHPUnit_Framework_TestCase
{
    /** @var  StoreApi */
    private $api;

    public function setUp()
    {
        $this->api = new Api\StoreApi();
    }

    public function testGetInventory()
    {
        $result = $this->api->getInventory();

        $this->assertInternalType('array', $result);
        $this->assertInternalType('int', $result['available']);
    }
}
