import 'package:openapi/api.dart';
import 'package:openapi/api/another_fake_api.dart';
import 'package:test/test.dart';


/// tests for AnotherFakeApi
void main() {
  final instance = Openapi().getAnotherFakeApi();

  group(AnotherFakeApi, () {
    // To test special tags
    //
    // To test special tags and operation ID starting with number
    //
    //Future<Client> 123test@$%SpecialTags(Client client) async
    test('test 123test@$%SpecialTags', () async {
      // TODO
    });

  });
}
