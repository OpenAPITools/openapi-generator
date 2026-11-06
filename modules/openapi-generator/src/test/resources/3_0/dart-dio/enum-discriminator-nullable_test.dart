// Runtime regression for enum-discriminator-nullable.yaml.
// Generate with pubName=enum_discriminator_test and enumUnknownDefaultCase=true,
// copy this file to the generated client's test directory, then run:
// dart pub get && dart run build_runner build && dart test
import 'package:enum_discriminator_test/enum_discriminator_test.dart';
import 'package:test/test.dart';

void main() {
  test('subtypes infer their optional enum and round-trip JSON', () {
    final models = <Entity>[DocumentEntity(), ImageEntity()];
    final kinds = [EntityKind.document, EntityKind.image];
    for (var i = 0; i < models.length; i++) {
      final model = models[i];
      expect(model.kind, kinds[i]);
      expect(model.discriminatorValue, kinds[i].name);
      final json = standardSerializers.serializeWith(Entity.serializer, model);
      expect((json as Map)['kind'], kinds[i].name);
      expect(
        standardSerializers.deserializeWith(Entity.serializer, json),
        model,
      );
    }
  });

  test('base model and builder retain their null discriminator', () {
    final base = $Entity();
    expect(base.kind, isNull);
    expect(base.discriminatorValue, isNull);
    expect($EntityBuilder().discriminatorValue, isNull);
  });

  test('unknown discriminator retains the configured enum fallback', () {
    final model = standardSerializers.deserializeWith(Entity.serializer, {
      'kind': 'unknown',
    });
    expect(model, isA<$Entity>());
    expect(model!.kind, EntityKind.unknownDefaultOpenApi);
    expect(model.discriminatorValue, isNull);
  });
}
