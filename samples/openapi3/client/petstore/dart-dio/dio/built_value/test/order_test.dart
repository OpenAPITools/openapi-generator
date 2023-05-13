import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for Order
void main() {
  final instance = OrderBuilder();
  // add properties to the builder and call build()

  group(Order, () {
    // int id
    test('to test the property `id`', () async {});

    // int petId
    test('to test the property `petId`', () async {});

    // int quantity
    test('to test the property `quantity`', () async {});

    // DateTime shipDate
    test('to test the property `shipDate`', () async {});

    // Order Status
    // String status
    test('to test the property `status`', () async {});

    // bool complete (default value: false)
    test('to test the property `complete`', () async {});
  });
}
