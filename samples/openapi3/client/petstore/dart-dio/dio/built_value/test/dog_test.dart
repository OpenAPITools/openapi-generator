import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for Dog
void main() {
  final instance = DogBuilder();
  // add properties to the builder and call build()

  group(Dog, () {
    // String className
    test('to test the property `className`', () async {});

    // String color (default value: 'red')
    test('to test the property `color`', () async {});

    // String breed
    test('to test the property `breed`', () async {});
  });
}
