<?php

require_once 'BaseApiTest.php';

class StoreApiTest extends BaseApiTest {

  public function testGetOrderById() {
    $res = $this->storeApi->getOrderById(1);
    $this->assertEquals(1, $res->petId);
    $this->assertEquals('DateTime', get_class($res->shipDate));
  }

  public function testDeleteOrder() {
    $res = $this->storeApi->deleteOrder(3);
    $res = $this->storeApi->deleteOrder("foo");

    // We just want to make sure there are no errors in this test.
    // To verify you are getting back a 200, you might want to add
    // something like this to Swagger.php callAPI():
    // print "Response for call to $resourcePath : ";
    // print_r($data);
  }

  public function testPlaceOrder() {
    $order = new Order();

    $order->petId = 1;
    $order->status = "ordered";
    $order->quantity = 10;
    // $order->shipDate = "1/1/2013";

    $res = $this->storeApi->placeOrder($order);

    // We just want to make sure there are no errors in this test.
    // To verify you are getting back a 200, you might want to add
    // something like this to Swagger.php callAPI():
    // print "Response for call to $resourcePath : ";
    // print_r($data);
  }

}
?>