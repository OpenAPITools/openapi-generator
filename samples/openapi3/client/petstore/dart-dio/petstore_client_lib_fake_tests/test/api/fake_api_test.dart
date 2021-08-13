import 'dart:typed_data';

import 'package:built_collection/built_collection.dart';
import 'package:dio/dio.dart';
import 'package:http_mock_adapter/http_mock_adapter.dart';
import 'package:openapi/api.dart';
import 'package:openapi/api/fake_api.dart';
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

  group(FakeApi, () {
    group('testEndpointParameters', () {
      test('complete', () async {
        server.onPost(
          '/fake',
          (request) => request.reply(200, null),
          data: {
            'number': '3',
            'double': '-13.57',
            'pattern_without_delimiter': 'patternWithoutDelimiter',
            'byte': '0',
            'float': '1.23',
            'integer': '45',
            'int32': '2147483647',
            'int64': '9223372036854775807',
            'date': '2020-08-11T00:00:00.000Z',
            'dateTime': '2020-08-11T12:30:55.123Z',
            'binary': "Instance of 'MultipartFile'",
          },
          headers: <String, dynamic>{
            'content-type': 'application/x-www-form-urlencoded',
            'content-length': 255,
          },
        );

        final response = await client.getFakeApi().testEndpointParameters(
              3,
              -13.57,
              'patternWithoutDelimiter',
              '0',
              float: 1.23,
              integer: 45,
              int32: 2147483647,
              int64: 9223372036854775807,
              date: DateTime.utc(2020, 8, 11),
              dateTime: DateTime.utc(2020, 8, 11, 12, 30, 55, 123),
              binary: Uint8List.fromList([0, 1, 2, 3, 4, 5]),
            );

        expect(response.statusCode, 200);
      });

      test('minimal', () async {
        server.onPost(
          '/fake',
          (request) => request.reply(200, null),
          data: {
            'byte': '0',
            'double': '-13.57',
            'number': '3',
            'pattern_without_delimiter': 'patternWithoutDelimiter',
          },
          headers: <String, dynamic>{
            'content-type': 'application/x-www-form-urlencoded',
            'content-length': 79,
          },
        );

        final response = await client.getFakeApi().testEndpointParameters(
              3,
              -13.57,
              'patternWithoutDelimiter',
              '0',
            );

        expect(response.statusCode, 200);
      });
    });

    group('testEnumParameters', () {
      test('in body data', () async {
        // Not sure if this is correct, we are not sending
        // form data in the body but some weird map
        server.onGet(
          '/fake',
          (request) => request.reply(200, null),
          data: {
            'enum_form_string': 'formString',
            'enum_form_string_array': '[foo, bar]',
          },
          headers: <String, dynamic>{
            'content-type': 'application/x-www-form-urlencoded',
          },
        );

        final response = await client.getFakeApi().testEnumParameters(
              enumFormString: 'formString',
              enumFormStringArray: ListBuilder<String>(
                <String>['foo', 'bar'],
              ).build(),
            );

        expect(response.statusCode, 200);
      });
    });
  });
}
