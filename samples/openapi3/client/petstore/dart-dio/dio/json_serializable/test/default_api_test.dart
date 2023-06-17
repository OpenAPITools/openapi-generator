import 'package:test/test.dart';
import 'package:openapi/openapi.dart';


/// tests for DefaultApi
void main() {
  final instance = Openapi().getDefaultApi();

  group(DefaultApi, () {
    //Future<FooBasicGetDefaultResponse> fooBasicGet() async
    test('test fooBasicGet', () async {
      // TODO
    });

    //Future<GigaOneOf> list() async
    test('test list', () async {
      // TODO
    });

    //Future<Fruit> oneofGet() async
    test('test oneofGet', () async {
      // TODO
    });

    //Future test({ Object body }) async
    test('test test', () async {
      // TODO
    });

    //Future<FruitVariant1> variant1Get() async
    test('test variant1Get', () async {
      // TODO
    });

    //Future<FruitAllOfDisc> variant2Get() async
    test('test variant2Get', () async {
      // TODO
    });

  });
}
