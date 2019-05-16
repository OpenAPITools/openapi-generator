// ref: https://dart.dev/guides/testing
//

part of openapi.tests;


/// tests for StoreApi
testStoreApi {
  var instance = new StoreApi();

  describe('tests for StoreApi', () {
    // Delete purchase order by ID
    //
    // For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    //
    //Future deleteOrder(String orderId) async 
    it('test deleteOrder', () async {
      // TODO
    });

    // Returns pet inventories by status
    //
    // Returns a map of status codes to quantities
    //
    //Future<Map<String, int>> getInventory() async 
    it('test getInventory', () async {
      // TODO
    });

    // Find purchase order by ID
    //
    // For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    //
    //Future<Order> getOrderById(int orderId) async 
    it('test getOrderById', () async {
      // TODO
    });

    // Place an order for a pet
    //
    // 
    //
    //Future<Order> placeOrder(Order body) async 
    it('test placeOrder', () async {
      // TODO
    });

  });
}
