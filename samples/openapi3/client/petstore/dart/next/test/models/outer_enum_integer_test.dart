import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';

void main() {
  group(r'OuterEnumInteger', () {
    
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
