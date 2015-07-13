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
  
}

?>

