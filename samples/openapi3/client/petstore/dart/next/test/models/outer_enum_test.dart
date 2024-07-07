import 'package:petstore_api/_internal.dart';
import 'package:test/test.dart';

void main() {
  group(r'OuterEnum', () {
    
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
