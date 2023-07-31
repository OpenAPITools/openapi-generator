import 'package:test/test.dart';
import 'package:openapi/openapi.dart';


/// tests for BarApi
void main() {
  final instance = Openapi().getBarApi();

  group(BarApi, () {
    // Create a Bar
    //
    //Future<Bar> createBar(BarCreate barCreate) async
    test('test createBar', () async {
      // TODO
    });

  });
}
