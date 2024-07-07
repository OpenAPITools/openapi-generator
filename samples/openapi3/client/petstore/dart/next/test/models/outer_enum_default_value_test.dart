import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';

void main() {
  group(r'OuterEnumDefaultValue', () {
    
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
