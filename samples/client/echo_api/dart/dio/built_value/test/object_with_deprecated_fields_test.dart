import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for ObjectWithDeprecatedFields
void main() {
  final instance = ObjectWithDeprecatedFieldsBuilder();
  // add properties to the builder and call build()

  group(ObjectWithDeprecatedFields, () {
    // String uuid
    test('to test the property `uuid`', () async {});

    // num id
    test('to test the property `id`', () async {});

    // DeprecatedObject deprecatedRef
    test('to test the property `deprecatedRef`', () async {});

    // BuiltList<Bar> bars
    test('to test the property `bars`', () async {});
  });
}
