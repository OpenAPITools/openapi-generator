import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for NullableClass
void main() {
  final instance = NullableClassBuilder();
  // add properties to the builder and call build()

  group(NullableClass, () {
    // int integerProp
    test('to test the property `integerProp`', () async {});

    // num numberProp
    test('to test the property `numberProp`', () async {});

    // bool booleanProp
    test('to test the property `booleanProp`', () async {});

    // String stringProp
    test('to test the property `stringProp`', () async {});

    // Date dateProp
    test('to test the property `dateProp`', () async {});

    // DateTime datetimeProp
    test('to test the property `datetimeProp`', () async {});

    // BuiltList<JsonObject> arrayNullableProp
    test('to test the property `arrayNullableProp`', () async {});

    // BuiltList<JsonObject> arrayAndItemsNullableProp
    test('to test the property `arrayAndItemsNullableProp`', () async {});

    // BuiltList<JsonObject> arrayItemsNullable
    test('to test the property `arrayItemsNullable`', () async {});

    // BuiltMap<String, JsonObject> objectNullableProp
    test('to test the property `objectNullableProp`', () async {});

    // BuiltMap<String, JsonObject> objectAndItemsNullableProp
    test('to test the property `objectAndItemsNullableProp`', () async {});

    // BuiltMap<String, JsonObject> objectItemsNullable
    test('to test the property `objectItemsNullable`', () async {});
  });
}
