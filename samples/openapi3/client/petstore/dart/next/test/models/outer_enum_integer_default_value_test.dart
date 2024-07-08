import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';

void main() {
  group(r'OuterEnumIntegerDefaultValue', () {
    test('values not empty', () {
      expect(OuterEnumIntegerDefaultValue.values, isNotEmpty);
    });

    test('unsafe', () {
      final example = 123;
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
