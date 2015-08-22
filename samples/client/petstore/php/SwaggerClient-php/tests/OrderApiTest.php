<?php

require_once('autoload.php');

class OrderApiTest extends \PHPUnit_Framework_TestCase
{

    // add a new pet (id 10005) to ensure the pet object is available for all the tests
    public static function setUpBeforeClass() {
        // for error reporting (need to run with php5.3 to get no warning)
        //ini_set('display_errors', 1);
        //error_reporting(~0);
    }
  
    // test get inventory
    public function testOrder()
    {
        // initialize the API client
        $order = new Swagger\Client\Model\Order();
  
        $order->setStatus("placed");
        $this->assertSame("placed", $order->getStatus());
    }
 
    /**
     * @expectedException InvalidArgumentException
     */ 
    public function testOrderException()
    {
        // initialize the API client
        $order = new Swagger\Client\Model\Order();
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
        $serializer = new Swagger\Client\ObjectSerializer;
        $order = $serializer->deserialize(json_decode($order_json), 'Swagger\Client\Model\Order');
        
        $this->assertInstanceOf('Swagger\Client\Model\Order', $order);
        $this->assertSame(10, $order->getId());
        $this->assertSame(20, $order->getPetId());
        $this->assertSame(30, $order->getQuantity());
        $this->assertTrue(new DateTime("2015-08-22T07:13:36.613Z") == $order->getShipDate());
        $this->assertSame("placed", $order->getStatus());
        $this->assertSame(false, $order->getComplete());
    }
  
}

?>

