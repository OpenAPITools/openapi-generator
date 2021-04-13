import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'random_id.dart';

void main() {
  var storeApi = new StoreApi();

  Order makeOrder({int id}) {
    return Order()
      ..id = id
      ..petId = 1234
      ..quantity = 1
      ..shipDate = DateTime.now()
      ..status
      ..complete = false;
  }

  group('Store API with live client', () {
    test('places an order and gets it by id', () async {
      // TODO: Please uncomment this after a content type of the petstore order in petstore.yaml is fixed.
      // var id = newId();

      // await storeApi.placeOrder(makeOrder(id: id));
      // var order = await storeApi.getOrderById(id);
      // expect(order.id, equals(id));
    });

    test('deletes an order', () async {
      // TODO: Please uncomment this after a content type of the petstore order in petstore.yaml is fixed.
      // var id = newId();

      // await storeApi.placeOrder(makeOrder(id: id));
      // await storeApi.deleteOrder(id.toString());
      // expect(storeApi.getOrderById(id),
      //     throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('gets the store inventory', () async {
      Map<String, int> inventory = await storeApi.getInventory();
      expect(inventory.length, isNot(equals(0)));
    });
  }); // , skip: 'e2e tests for CI'
}
