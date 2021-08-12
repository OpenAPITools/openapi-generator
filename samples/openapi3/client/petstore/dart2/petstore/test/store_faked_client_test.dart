import 'dart:io';

import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'fake_client.dart';
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

  group('Store API with faked client', () {
    test('places an order and gets it by id', () async {
      final id = newId();
      final newOrder = makeOrder(id: id);

      // use the store api to add an order
      storeApi.apiClient.client = FakeClient(
          expectedUrl: 'http://petstore.swagger.io/v2/store/order',
          expectedPostRequestBody: await storeApi.apiClient.serializeAsync(newOrder),
          postResponseBody: await storeApi.apiClient.serializeAsync(newOrder),
          expectedHeaders: {"Content-Type": "application/json"});
      await storeApi.placeOrder(newOrder);

      // retrieve the same order by id
      storeApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/store/order/$id',
        getResponseBody: await storeApi.apiClient.serializeAsync(newOrder),
      );
      final placedOrder = await storeApi.getOrderById(id);
      expect(placedOrder.id, equals(id));
    });

    test('deletes an order', () async {
      final id = newId();
      final newOrder = makeOrder(id: id);

      // use the store api to add an order
      storeApi.apiClient.client = FakeClient(
          expectedUrl: 'http://petstore.swagger.io/v2/store/order',
          expectedPostRequestBody: await storeApi.apiClient.serializeAsync(newOrder),
          postResponseBody: await storeApi.apiClient.serializeAsync(newOrder),
          expectedHeaders: {"Content-Type": "application/json"});
      await storeApi.placeOrder(newOrder);

      // delete the same order
      storeApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/store/order/$id',
        deleteResponseBody: '',
      );
      await storeApi.deleteOrder(id.toString());

      // try and retrieve the order
      storeApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/store/order/$id',
        throwException: ApiException(400, 'Not found'),
      );
      expect(storeApi.getOrderById(id), throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('gets the store inventory', () async {
      // get some test data (recorded from live response)
      final inventoryResponse = await File('test/inventory_response.json').readAsString();
      // use the store api to get the inventory
      storeApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/store/inventory',
        getResponseBody: inventoryResponse,
      );
      Map<String, int> inventory = await storeApi.getInventory();
      expect(inventory.length, isNot(equals(0)));
    });
  });
}
