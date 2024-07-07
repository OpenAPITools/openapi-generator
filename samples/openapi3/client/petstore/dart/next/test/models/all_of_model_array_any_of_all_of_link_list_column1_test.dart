import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';

void main() {
  group(r'AllOfModelArrayAnyOfAllOfLinkListColumn1', () {
    
    final reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1.$reflection;

    final exampleInstance = reflection.example();
    Object? doSerialize() {
      final result = exampleInstance.serialize();
      return result;
    }
    test('serialize', () {
      final serialized = doSerialize();
      expect(serialized, isNotNull);
    });

    test('validate', () {
      expect(exampleInstance.validate(), isTrue);
    });
    test('deserialize', () {
      final serialized = doSerialize();
      final deserialized = AllOfModelArrayAnyOfAllOfLinkListColumn1.deserialize(serialized);
      expect(deserialized.validate(), isTrue);
      expect(deserialized.serialize(), serialized);
    });
  });
}
