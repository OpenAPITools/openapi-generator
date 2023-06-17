import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for Cat
void main() {
  final instance = CatBuilder();
  // add properties to the builder and call build()

  group(Cat, () {
    // String className
    test('to test the property `className`', () async {});

    // String color (default value: 'red')
    test('to test the property `color`', () async {});

    // bool declawed
    test('to test the property `declawed`', () async {});
  });
}
