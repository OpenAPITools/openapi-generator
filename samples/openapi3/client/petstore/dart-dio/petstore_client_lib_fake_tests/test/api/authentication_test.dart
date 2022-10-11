import 'package:dio/dio.dart';
import 'package:http_mock_adapter/http_mock_adapter.dart';
import 'package:openapi/openapi.dart';
import 'package:test/test.dart';

void main() {
  Openapi client;
  DioAdapter tester;

  setUp(() {
    client = Openapi(dio: Dio());
    tester = DioAdapter(dio: client.dio);
  });

  tearDown(() {
    tester.close();
  });

  group('Authentication', () {
    test('http_basic', () async {
      client.setBasicAuth('http_basic_test', 'foo', 'bar');

      tester.onPost(
        '/fake',
        (server) => server.reply(200, null),
        data: {
          'number': '1',
          'double': '1.1',
          'pattern_without_delimiter': 'pattern',
          'byte': '1',
        },
        headers: <String, dynamic>{
          'content-type': 'application/x-www-form-urlencoded',
          'content-length': Matchers.integer,
          'authorization': Matchers.string,
        },
      );

      final response = await client.getFakeApi().testEndpointParameters(
            number: 1,
            double_: 1.1,
            patternWithoutDelimiter: 'pattern',
            byte: '1',
          );

      expect(response.statusCode, 200);
    });

    test('bearer', () async {
      client.setBearerAuth('bearer_test', 'foobar');

      tester.onDelete(
        '/fake',
        (server) => server.reply(200, null),
        headers: <String, dynamic>{
          'required_boolean_group': 'false',
          'authorization': Matchers.pattern('Bearer foobar'),
        },
        queryParameters: <String, dynamic>{
          'required_string_group': 1,
          'required_int64_group': 2,
        },
      );

      final response = await client.getFakeApi().testGroupParameters(
            requiredStringGroup: 1,
            requiredBooleanGroup: false,
            requiredInt64Group: 2,
          );

      expect(response.statusCode, 200);
    });

    test('api_key', () async {
      client.setApiKey('api_key', 'SECRET_API_KEY');

      tester.onGet(
        '/store/inventory',
        (server) => server.reply(200, {
          'foo': 999,
        }),
        headers: <String, dynamic>{
          'api_key': 'SECRET_API_KEY',
        },
      );

      final response = await client.getStoreApi().getInventory();

      expect(response.statusCode, 200);
    });
  });
}
