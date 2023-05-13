import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for MixedPropertiesAndAdditionalPropertiesClass
void main() {
  final instance = MixedPropertiesAndAdditionalPropertiesClassBuilder();
  // add properties to the builder and call build()

  group(MixedPropertiesAndAdditionalPropertiesClass, () {
    // String uuid
    test('to test the property `uuid`', () async {});

    // DateTime dateTime
    test('to test the property `dateTime`', () async {});

    // BuiltMap<String, Animal> map
    test('to test the property `map`', () async {});
  });
}
