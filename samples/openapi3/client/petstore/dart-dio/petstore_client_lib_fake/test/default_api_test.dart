import 'package:openapi/api.dart';
import 'package:openapi/api/default_api.dart';
import 'package:test/test.dart';


/// tests for DefaultApi
void main() {
  final instance = Openapi().getDefaultApi();

  group(DefaultApi, () {
    //Future<InlineResponseDefault> fooGet() async
    test('test fooGet', () async {
      // TODO
    });

  });
}
