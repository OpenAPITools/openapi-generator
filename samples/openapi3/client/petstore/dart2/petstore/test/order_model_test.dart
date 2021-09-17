import 'package:openapi/api.dart';
import 'package:test/test.dart';

void main() {
  test('Check if default value is generated', () async {
    var order = Order();
    expect(order.complete, equals(false));
  });
}
