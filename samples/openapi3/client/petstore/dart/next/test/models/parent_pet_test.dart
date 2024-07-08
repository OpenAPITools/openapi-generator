import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';

void main() {
  group(r'ParentPet', () {

    final reflection = ParentPet.$reflection;

    late ParentPet exampleInstance;
    setUp(() {
      exampleInstance = reflection.example();
    });

    Object? doSerialize() {
      final result = exampleInstance.serialize();
      return result;
    }
    test('serialize', () {
      expect(exampleInstance, isNotNull);
      final serialized = doSerialize();
      expect(serialized, isNotNull);
    });

    test('validate', () {
      expect(exampleInstance, isNotNull);
      expect(exampleInstance.validate(), isTrue);
    });
    test('deserialize', () {
      expect(exampleInstance, isNotNull);
      final serialized = doSerialize();
      final deserialized = ParentPet.deserialize(serialized);
      expect(deserialized.validate(), isTrue);
      expect(deserialized.serialize(), serialized);
    });
  });
}
