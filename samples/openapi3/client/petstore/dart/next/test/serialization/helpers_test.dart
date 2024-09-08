import '../utils.dart';
import 'package:parameterized_test/parameterized_test.dart';
import 'package:test/test.dart';
import 'package:petstore_api/_internal.dart';

void main() {
  group('Serialization Helpers', () {
    parameterizedTest(
      'Uint8ListReflection.isBase64String',
      [
        ['invalid string', false, null],
        ['SGVsbG8gd29ybGQgIQ==', true, 'Hello world !'],
        ['IA==', true, ' '],
      ],
      (String input, bool result, String? original) {
        expect(Uint8ListReflection.isBase64String(input), result);
        if (result) {
          expect(utf8.decode(base64.decode(input)), original);
        }
      },
    );
  });

  group('XML Reflection', () {
    test(
      'test wrapped array',
      () {
        final rootXml = XmlReflection(
          prefix: r'kl',
          wrapped: true,
        );
        final propReflection = UndefinedWrapperReflection(XmlReflectionWrapper(
          xml: rootXml,
          ListReflection(XmlReflectionWrapper(
            xml: XmlReflection(prefix: r'mn', xmlName: r'my_item'),
            PrimitiveReflection.forint,
          )),
        ));
        final context = XmlSerializationContext()
            .withOasNameContainer(OasNameWrapper(oasName: 'my_list'));
        final serialized1 = context.handleAttributes(
          rootXml,
          context.wrapSerializedValue(
            rootXml.getQualifiedName('my_list'),
            propReflection.serialize(
              UndefinedWrapper(<int>[]),
              context,
            ),
          ),
        );
        print(serialized1);
        final serialized2 = context.handleAttributes(
          rootXml,
          context.wrapSerializedValue(
            rootXml.getQualifiedName('my_list'),
            propReflection.serialize(
              UndefinedWrapper(<int>[1, 2, 3]),
              context,
            ),
          ),
        );
        print(serialized2);
      },
    );
  });
}