import 'package:test/test.dart';
import 'package:openapi/openapi.dart';


/// tests for FooApi
void main() {
  final instance = Openapi().getFooApi();

  group(FooApi, () {
    // Create a Foo
    //
    //Future<FooRefOrValue> createFoo({ Foo foo }) async
    test('test createFoo', () async {
      // TODO
    });

    // GET all Foos
    //
    //Future<BuiltList<FooRefOrValue>> getAllFoos() async
    test('test getAllFoos', () async {
      // TODO
    });

  });
}
