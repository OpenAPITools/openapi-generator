import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

/// tests for DefaultApi
void main() {
  final instance = Openapi().getDefaultApi();

  group(DefaultApi, () {
    //Future<FooBasicGetDefaultResponse> fooBasicGet() async
    test('test fooBasicGet', () async {});

    //Future<GigaOneOf> list() async
    test('test list', () async {});

    //Future<Fruit> oneofGet() async
    test('test oneofGet', () async {});

    //Future test({ JsonObject body }) async
    test('test test', () async {});

    //Future<FruitVariant1> variant1Get() async
    test('test variant1Get', () async {});

    //Future<FruitAllOfDisc> variant2Get() async
    test('test variant2Get', () async {});
  });
}
