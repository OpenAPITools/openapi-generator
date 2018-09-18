import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'random_id.dart';

void main() {
  var storeApi = new StoreApi();

  group('Store API ', () {
    test('places an order and gets it by id', () async {
      var id = newId();

      await storeApi.placeOrder(new Order()..id = id);
      var order = await storeApi.getOrderById(id);
      expect(order.id, equals(id));
    });

    test('deletes an order', () async {
      var id = newId();

      await storeApi.placeOrder(new Order()..id = id);
      await storeApi.deleteOrder(id.toString());
      expect(storeApi.getOrderById(id), throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('gets the store inventory', () async {
      Map<String, int> inventory = await storeApi.getInventory();
      expect(inventory.length, isNot(equals(0)));
    });
  });
}
