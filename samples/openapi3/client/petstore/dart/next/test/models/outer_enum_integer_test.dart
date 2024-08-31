import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';
import '../utils.dart';
import 'package:parameterized_test/parameterized_test.dart';

void main() {
  group(r'OuterEnumInteger', () {
    final reflection = OuterEnumInteger.$reflection;
    final exampleContext = ExampleContext();
      test('values not empty', () {
        expect(OuterEnumInteger.values, isNotEmpty);
      });

      test('unsafe', () {
        final example = reflection.subReflection.exampleFunction(exampleContext);
        expect(OuterEnumInteger.$unsafe(example), example);
      });

      
      test(r'number0', () {
        expect(OuterEnumInteger.number0().value, 0);
      });
      
      test(r'number1', () {
        expect(OuterEnumInteger.number1().value, 1);
      });
      
      test(r'number2', () {
        expect(OuterEnumInteger.number2().value, 2);
      });
      
  });
}
