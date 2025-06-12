import 'package:test/test.dart';
import 'package:openapi/openapi.dart';


/// tests for DefaultApi
void main() {
  final instance = Openapi().getDefaultApi();

  group(DefaultApi, () {
    //Future<Uint8List> binaryResponse() async
    test('test binaryResponse', () async {
      // TODO
    });

  });
}
