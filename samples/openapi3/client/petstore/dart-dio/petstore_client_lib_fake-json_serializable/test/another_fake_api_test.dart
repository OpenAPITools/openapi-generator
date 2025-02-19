import 'package:test/test.dart';
import 'package:openapi/openapi.dart';


/// tests for AnotherFakeApi
void main() {
  final instance = Openapi().getAnotherFakeApi();

  group(AnotherFakeApi, () {
    // To test special tags
    //
    // To test special tags and operation ID starting with number
    //
    //Future<ModelClient> call123testSpecialTags(ModelClient modelClient) async
    test('test call123testSpecialTags', () async {
      // TODO
    });

  });
}
