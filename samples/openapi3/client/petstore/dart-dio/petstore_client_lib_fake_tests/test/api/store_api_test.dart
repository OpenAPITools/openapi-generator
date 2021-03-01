import 'package:dio/dio.dart';
import 'package:http_mock_adapter/http_mock_adapter.dart';
import 'package:openapi/api.dart';
import 'package:openapi/api/store_api.dart';
import 'package:test/test.dart';

void main() {
  Openapi client;
  DioAdapter server;

  setUp(() {
    server = DioAdapter();
    client = Openapi(dio: Dio()..httpClientAdapter = server);
  });

  tearDown(() {
    server.close();
  });

  group(StoreApi, () {
    group('getInventory', () {
      test('with API key', () async {
        client.setApiKey('api_key', 'SECRET_API_KEY');

        server.onGet(
          '/store/inventory',
          headers: <String, dynamic>{
            'api_key': 'SECRET_API_KEY',
          },
          handler: (response) => response.reply(200, {
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
  });
}
