import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';
import '../utils.dart';
import 'package:parameterized_test/parameterized_test.dart';

void main() {
  group(r'OuterEnumIntegerDefaultValue', () {
    final reflection = OuterEnumIntegerDefaultValue.$reflection;
    final exampleContext = ExampleContext();
      test('values not empty', () {
        expect(OuterEnumIntegerDefaultValue.values, isNotEmpty);
      });

      test('unsafe', () {
        final example = reflection.subReflection.exampleFunction(exampleContext);
        expect(OuterEnumIntegerDefaultValue.$unsafe(example), example);
      });

      
      test(r'number0', () {
        expect(OuterEnumIntegerDefaultValue.number0().value, 0);
      });
      
      test(r'number1', () {
        expect(OuterEnumIntegerDefaultValue.number1().value, 1);
      });
      
      test(r'number2', () {
        expect(OuterEnumIntegerDefaultValue.number2().value, 2);
      });
      
  });
}
