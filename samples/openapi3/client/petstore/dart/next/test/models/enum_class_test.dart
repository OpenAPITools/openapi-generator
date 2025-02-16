import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';
import '../utils.dart';
import 'package:parameterized_test/parameterized_test.dart';

void main() {
  group(r'EnumClass', () {
    final reflection = EnumClass.$reflection;
    final exampleContext = ExampleContext();
      test('values not empty', () {
        expect(EnumClass.values, isNotEmpty);
      });

      test('unsafe', () {
        final example = reflection.subReflection.exampleFunction(exampleContext);
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
