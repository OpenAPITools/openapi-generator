import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for Fruit
void main() {
  final instance = FruitBuilder();
  // add properties to the builder and call build()

  group(Fruit, () {
    // String color
    test('to test the property `color`', () async {});

    // String kind
    test('to test the property `kind`', () async {});

    // num count
    test('to test the property `count`', () async {});
  });
}
