<?php

namespace OpenAPI\Client;

use PHPUnit\Framework\TestCase;

class OrderApiTest extends TestCase
{

    // add a new pet (id 10005) to ensure the pet object is available for all the tests
    public static function setUpBeforeClass()
    {
        // for error reporting (need to run with php5.3 to get no warning)
        //ini_set('display_errors', 1);
        //error_reporting(~0);
    }
  
    // test get inventory
    public function testOrderEnum()
    {
        $this->assertSame(Model\Order::STATUS_PLACED, "placed");
        $this->assertSame(Model\Order::STATUS_APPROVED, "approved");
    }

    // test get inventory
    public function testOrder()
    {
        // initialize the API client
        $order = new Model\Order();
  
        $order->setStatus("placed");
        $this->assertSame("placed", $order->getStatus());
    }
 
    /**
     * @expectedException InvalidArgumentException
     */
    public function testOrderException()
    {
        // initialize the API client
        $order = new Model\Order();
        $order->setStatus("invalid_value");
    }

    // test deseralization of order
    public function testDeserializationOfOrder()
    {
        $order_json = <<<ORDER
{
  "id": 10,
  "petId": 20,
  "quantity": 30,
  "shipDate": "2015-08-22T07:13:36.613Z",
  "status": "placed",
  "complete": false
}
ORDER;
        $order = ObjectSerializer::deserialize(
            json_decode($order_json),
            'OpenAPI\Client\Model\Order'
        );
        
        $this->assertInstanceOf('OpenAPI\Client\Model\Order', $order);
        $this->assertSame(10, $order->getId());
        $this->assertSame(20, $order->getPetId());
        $this->assertSame(30, $order->getQuantity());
        $this->assertTrue(new \DateTime("2015-08-22T07:13:36.613Z") == $order->getShipDate());
        $this->assertSame("placed", $order->getStatus());
        $this->assertSame(false, $order->getComplete());
    }
  
    // test deseralization of array of array of order
    public function testDeserializationOfArrayOfArrayOfOrder()
    {
        $order_json = <<<ORDER
[[{
  "id": 10,
  "petId": 20,
  "quantity": 30,
  "shipDate": "2015-08-22T07:13:36.613Z",
  "status": "placed",
  "complete": false
}]]
ORDER;
        $order = ObjectSerializer::deserialize(
            json_decode($order_json),
            'OpenAPI\Client\Model\Order[][]'
        );

        $this->assertArrayHasKey(0, $order);
        $this->assertArrayHasKey(0, $order[0]);
        $_order = $order[0][0];
        $this->assertInstanceOf('OpenAPI\Client\Model\Order', $_order);
        $this->assertSame(10, $_order->getId());
        $this->assertSame(20, $_order->getPetId());
        $this->assertSame(30, $_order->getQuantity());
        $this->assertTrue(new \DateTime("2015-08-22T07:13:36.613Z") == $_order->getShipDate());
        $this->assertSame("placed", $_order->getStatus());
        $this->assertSame(false, $_order->getComplete());
    }

    // test deseralization of map of map of order
    public function testDeserializationOfMapOfMapOfOrder()
    {
        $order_json = <<<ORDER
{
  "test": {
    "test2": {
      "id": 10,
      "petId": 20,
      "quantity": 30,
      "shipDate": "2015-08-22T07:13:36.613Z",
      "status": "placed",
      "complete": false
    }
  }
}
ORDER;
        $order = ObjectSerializer::deserialize(
            json_decode($order_json),
            'map[string,map[string,\OpenAPI\Client\Model\Order]]'
        );

        $this->assertArrayHasKey('test', $order);
        $this->assertArrayHasKey('test2', $order['test']);
        $_order = $order['test']['test2'];
        $this->assertInstanceOf('OpenAPI\Client\Model\Order', $_order);
        $this->assertSame(10, $_order->getId());
        $this->assertSame(20, $_order->getPetId());
        $this->assertSame(30, $_order->getQuantity());
        $this->assertTrue(new \DateTime("2015-08-22T07:13:36.613Z") == $_order->getShipDate());
        $this->assertSame("placed", $_order->getStatus());
        $this->assertSame(false, $_order->getComplete());
    }
}
