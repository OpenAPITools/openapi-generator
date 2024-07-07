import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';

void main() {
  group(r'NewPetCategoryInlineAllofAllOfCategoryTag', () {
    
    final reflection = NewPetCategoryInlineAllofAllOfCategoryTag.$reflection;

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
      final deserialized = NewPetCategoryInlineAllofAllOfCategoryTag.deserialize(serialized);
      expect(deserialized.validate(), isTrue);
      expect(deserialized.serialize(), serialized);
    });
  });
}
