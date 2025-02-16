import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';
import '../utils.dart';
import 'package:parameterized_test/parameterized_test.dart';

void main() {
  group(r'OuterEnum', () {
    final reflection = OuterEnum.$reflection;
    final exampleContext = ExampleContext();
      test('values not empty', () {
        expect(OuterEnum.values, isNotEmpty);
      });

      test('unsafe', () {
        final example = reflection.subReflection.exampleFunction(exampleContext);
        expect(OuterEnum.$unsafe(example), example);
      });

      
      test(r'placed', () {
        expect(OuterEnum.placed().value, r'placed');
      });
      
      test(r'approved', () {
        expect(OuterEnum.approved().value, r'approved');
      });
      
      test(r'delivered', () {
        expect(OuterEnum.delivered().value, r'delivered');
      });
      
      test(r'LOWER_CASE_S', () {
        expect(OuterEnum.LOWER_CASE_S().value, r's');
      });
      
      test(r'UPPER_CASE_S', () {
        expect(OuterEnum.UPPER_CASE_S().value, r'S');
      });
      
  });
}
