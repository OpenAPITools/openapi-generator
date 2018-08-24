part of tests;

testStoreApi() {
  var storeApi = new StoreApi();

  describe('Store API ', () {
    it('places an order and gets it by id', () async {
      var id = newId();

      await storeApi.placeOrder(new Order()..id = id);
      var order = await storeApi.getOrderById(id);
      expect(order.id).toEqual(id);
    });

    it('deletes an order', () async {
      var id = newId();

      await storeApi.placeOrder(new Order()..id = id);
      await storeApi.deleteOrder(id.toString());
      expect(storeApi.getOrderById(id)).toThrowWith(anInstanceOf: ApiException);
    });

    it('gets the store inventory', () async {
      Map<String, int> inventory = await storeApi.getInventory();
      expect(inventory.length).not.toBe(0);
    });
  });
}
