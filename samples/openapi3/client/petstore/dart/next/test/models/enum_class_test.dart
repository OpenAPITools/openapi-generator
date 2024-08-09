import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';

void main() {
  group(r'EnumClass', () {
    test('values not empty', () {
      expect(EnumClass.values, isNotEmpty);
    });

    test('unsafe', () {
      final example = 'abc';
      expect(EnumClass.$unsafe(example), example);
    });

    
    test(r'abc', () {
      expect(EnumClass.abc().value, r'_abc');
    });
    
    test(r'efg', () {
      expect(EnumClass.efg().value, r'-efg');
    });
    
    test(r'xyz', () {
      expect(EnumClass.xyz().value, r'(xyz)');
    });
    
  });
}
