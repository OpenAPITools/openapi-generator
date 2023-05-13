import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for Pet
void main() {
  final instance = PetBuilder();
  // add properties to the builder and call build()

  group(Pet, () {
    // int id
    test('to test the property `id`', () async {});

    // Category category
    test('to test the property `category`', () async {});

    // String name
    test('to test the property `name`', () async {});

    // BuiltSet<String> photoUrls
    test('to test the property `photoUrls`', () async {});

    // BuiltList<Tag> tags
    test('to test the property `tags`', () async {});

    // pet status in the store
    // String status
    test('to test the property `status`', () async {});
  });
}
