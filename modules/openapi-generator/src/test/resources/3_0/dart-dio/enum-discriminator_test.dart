// Runtime regression shared by enum-discriminator.yaml and
// enum-discriminator-inline.yaml. Generate with pubName=enum_discriminator_test,
// copy this file to the generated client's test directory, then run:
// dart pub get && dart run build_runner build && dart test
import 'package:enum_discriminator_test/enum_discriminator_test.dart';
import 'package:test/test.dart';

void main() {
  test(
    'wire values initialize renamed enum constants without changing getters',
    () {
      final a = MerchantDetailsTypeADto((b) => b..uuid = 'a');
      final b = MerchantDetailsTypeBDto((b) => b..uuid = 'b');
      expect(a.merchantType.name, 'merchantA');
      expect(b.merchantType.name, 'merchantB');
      expect(a.discriminatorValue, 'type-a');
      expect(b.toBuilder().discriminatorValue, 'type/b');
    },
  );

  test('both wire values select their subtype and round-trip JSON', () {
    for (final wire in ['type-a', 'type/b']) {
      final model = standardSerializers.deserializeWith(
        MerchantDetailsDto.serializer,
        {'uuid': 'a', 'merchantType': wire},
      );
      expect(
        model,
        wire == 'type-a'
            ? isA<MerchantDetailsTypeADto>()
            : isA<MerchantDetailsTypeBDto>(),
      );
      expect(model!.discriminatorValue, wire);
      final json = standardSerializers.serializeWith(
        MerchantDetailsDto.serializer,
        model,
      );
      expect((json as Map)['merchantType'], wire);
    }
  });

  test('an unmapped base builder retains its null discriminator getter', () {
    expect($MerchantDetailsDtoBuilder().discriminatorValue, isNull);
  });
}
