import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';
import '../utils.dart';
import 'package:parameterized_test/parameterized_test.dart';

void main() {
  group(r'MixedPropertiesAndAdditionalPropertiesClass', () {
    final reflection = MixedPropertiesAndAdditionalPropertiesClass.$reflection;
    final exampleContext = ExampleContext();

      late MixedPropertiesAndAdditionalPropertiesClass exampleInstance;
      setUp(() {
        exampleInstance = reflection.example(exampleContext);
      });

      test('validate', () {
        expect(exampleInstance, isNotNull);
        expect(exampleInstance.validate(), isTrue);
      });

      parameterizedTest(
        'serialization roundtrip',
        [
          SerializationContext.json(
            fileBytesResolver: (file) => exampleContext.fileCache[file.name],
          ),
          SerializationContext.xml(
            fileBytesResolver: (file) => exampleContext.fileCache[file.name],
          ),
        ],
        (SerializationContext context) {
          final serialized = exampleInstance.serialize(context);
          final deserialized = MixedPropertiesAndAdditionalPropertiesClass.deserialize(serialized, context);
          expect(deserialized.validate(), isTrue);
          expect(deserialized.serialize(context), serialized);
        }
      );
  });
}
