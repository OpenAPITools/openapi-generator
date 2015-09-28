part of tests;

testStoreApi() {
  var storeApi = new StoreApi();

  describe('Store API ', () async {

    it('places an order and gets it by id', () async {
      var id = 4356;

      await storeApi.placeOrder(new Order()..id = id);
      var order = await storeApi.getOrderById(id.toString());
      expect(order.id).toEqual(id);
    });

    it('deletes an order', () async {
      var id = 637211;

      await storeApi.placeOrder(new Order()..id = id);
      await storeApi.deleteOrder(id.toString());
      expect(storeApi.getOrderById(id.toString())).toThrowWith(anInstanceOf: ApiException);
    });

    it('gets the store inventory', () async {
      Map<String, int> inventory = await storeApi.getInventory();
      expect(inventory.length).not.toBe(0);
    });

  });
}