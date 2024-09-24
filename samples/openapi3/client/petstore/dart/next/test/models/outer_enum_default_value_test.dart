import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';
import '../utils.dart';
import 'package:parameterized_test/parameterized_test.dart';

void main() {
  group(r'OuterEnumDefaultValue', () {
    final reflection = OuterEnumDefaultValue.$reflection;
    final exampleContext = ExampleContext();
      test('values not empty', () {
        expect(OuterEnumDefaultValue.values, isNotEmpty);
      });

      test('unsafe', () {
        final example = reflection.subReflection.exampleFunction(exampleContext);
        expect(OuterEnumDefaultValue.$unsafe(example), example);
      });

      
      test(r'placed', () {
        expect(OuterEnumDefaultValue.placed().value, r'placed');
      });
      
      test(r'approved', () {
        expect(OuterEnumDefaultValue.approved().value, r'approved');
      });
      
      test(r'delivered', () {
        expect(OuterEnumDefaultValue.delivered().value, r'delivered');
      });
      
  });
}
