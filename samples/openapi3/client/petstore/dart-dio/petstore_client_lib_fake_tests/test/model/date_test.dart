import 'package:openapi/openapi.dart';
import 'package:test/test.dart';

void main() {
  final date1 = Date(1999, 3, 25);
  final date2 = Date(1999, 1, 1);

  group(Date, () {
    test('toString', () {
      expect(date1.toString(), '1999-03-25');
    });

    test('compare >', () {
      expect(date1.compareTo(date2), 1);
    });

    test('compare <', () {
      expect(date2.compareTo(date1), -1);
    });

    test('compare =', () {
      expect(date1.compareTo(date1), 0);
    });
  });
}
