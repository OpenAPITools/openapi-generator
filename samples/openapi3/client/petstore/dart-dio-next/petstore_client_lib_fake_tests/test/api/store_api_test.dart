import 'package:dio/dio.dart';
import 'package:http_mock_adapter/http_mock_adapter.dart';
import 'package:openapi/openapi.dart';
import 'package:test/test.dart';

void main() {
  Openapi client;
  DioAdapter server;

  setUp(() {
    client = Openapi(dio: Dio());
    server = DioAdapter.configure(dio: client.dio);
  });

  tearDown(() {
    server.close();
  });

  group(StoreApi, () {
    test('getInventory', () async {
      server.onGet(
        '/store/inventory',
        (request) => request.reply(200, {
          'foo': 5,
          'bar': 999,
          'baz': 0,
        }),
      );

      final response = await client.getStoreApi().getInventory();

      expect(response.statusCode, 200);
      expect(response.data, isNotNull);
      expect(response.data.length, 3);
    });
  });
}
