import 'package:openapi/openapi.dart';
import 'package:openapi/src/date_serializer.dart';
import 'package:test/test.dart';

void main() {
  final date1 = Date(1999, 3, 25);
  const serializer = DateSerializer();

  group(DateSerializer, () {
    test('serialize', () {
      expect(
        serializer.serialize(serializers, date1),
        '1999-03-25',
      );
    });

    test('deserialize date', () {
      expect(
        serializer.deserialize(serializers, '1999-03-25'),
        date1,
      );
    });

    test('deserialize ISO', () {
      expect(
        serializer.deserialize(serializers, '1999-03-25T12:30:55.123Z'),
        date1,
      );
    });
  });
}
