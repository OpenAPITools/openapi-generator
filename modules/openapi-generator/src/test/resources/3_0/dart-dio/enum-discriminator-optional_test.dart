// Runtime regression for enum-discriminator-nullable.yaml with useOptional=true.
// Generate with pubName=enum_discriminator_test, copy to the client's test
// directory, then run dart pub get, dart run build_runner build and dart test.
import 'package:enum_discriminator_test/enum_discriminator_test.dart';
import 'package:test/test.dart';

void main() {
  test('wrapped subtype discriminator defaults retain their absent state', () {
    for (final model in <Entity>[DocumentEntity(), ImageEntity()]) {
      expect(model.kind.isEmpty, isTrue);
    }
    expect(DocumentEntity().discriminatorValue, 'document');
    expect(ImageEntity().discriminatorValue, 'image');
  });

  test(
    'explicit wire discriminators deserialize and round-trip wrapped enums',
    () {
      for (final kind in [EntityKind.document, EntityKind.image]) {
        final model = standardSerializers.deserializeWith(Entity.serializer, {
          'kind': kind.name,
        })!;
        expect(
          model,
          kind == EntityKind.document
              ? isA<DocumentEntity>()
              : isA<ImageEntity>(),
        );
        expect(model.kind.isPresent, isTrue);
        expect(model.kind.value, kind);
        final json = standardSerializers.serializeWith(
          Entity.serializer,
          model,
        );
        expect((json as Map)['kind'], kind.name);
      }
    },
  );
}
