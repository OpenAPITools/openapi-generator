import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for EntityRef
void main() {
  //final instance = EntityRefBuilder();
  // add properties to the builder and call build()

  group(EntityRef, () {
    // Name of the related entity.
    // String name
    test('to test the property `name`', () async {});

    // The actual type of the target instance when needed for disambiguation.
    // String atReferredType
    test('to test the property `atReferredType`', () async {});

    // Hyperlink reference
    // String href
    test('to test the property `href`', () async {});

    // unique identifier
    // String id
    test('to test the property `id`', () async {});

    // A URI to a JSON-Schema file that defines additional attributes and relationships
    // String atSchemaLocation
    test('to test the property `atSchemaLocation`', () async {});

    // When sub-classing, this defines the super-class
    // String atBaseType
    test('to test the property `atBaseType`', () async {});

    // When sub-classing, this defines the sub-class Extensible name
    // String atType
    test('to test the property `atType`', () async {});
  });
}
