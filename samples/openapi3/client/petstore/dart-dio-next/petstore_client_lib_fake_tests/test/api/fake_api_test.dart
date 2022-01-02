import 'dart:typed_data';

import 'package:built_collection/built_collection.dart';
import 'package:dio/dio.dart';
import 'package:dio/src/parameter.dart';
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

  group(FakeApi, () {
    group('testEndpointParameters', () {
      test('complete', () async {
        tester.onPost(
          '/fake',
          (server) => server.reply(200, null),
          data: {
            'number': '3',
            'double': '-13.57',
            'pattern_without_delimiter': 'patternWithoutDelimiter',
            'byte': '0',
            'float': '1.23',
            'integer': '45',
            'int32': '2147483647',
            'int64': '9223372036854775807',
            'date': '2020-08-11',
            'dateTime': '2020-08-11T12:30:55.123Z',
            'binary': '[0, 1, 2, 3, 4, 5]',
          },
          headers: <String, dynamic>{
            'content-type': 'application/x-www-form-urlencoded',
            'content-length': Matchers.integer,
          },
        );

        final response = await client.getFakeApi().testEndpointParameters(
              number: 3,
              double_: -13.57,
              patternWithoutDelimiter: 'patternWithoutDelimiter',
              byte: '0',
              float: 1.23,
              integer: 45,
              int32: 2147483647,
              int64: 9223372036854775807,
              date: Date(2020, 8, 11),
              dateTime: DateTime.utc(2020, 8, 11, 12, 30, 55, 123),
              binary: Uint8List.fromList([0, 1, 2, 3, 4, 5]),
            );

        expect(response.statusCode, 200);
      });

      test('minimal', () async {
        tester.onPost(
          '/fake',
          (server) => server.reply(200, null),
          data: {
            'byte': '0',
            'double': '-13.57',
            'number': '3',
            'pattern_without_delimiter': 'patternWithoutDelimiter',
          },
          headers: <String, dynamic>{
            'content-type': 'application/x-www-form-urlencoded',
            'content-length': Matchers.integer,
          },
        );

        final response = await client.getFakeApi().testEndpointParameters(
              number: 3,
              double_: -13.57,
              patternWithoutDelimiter: 'patternWithoutDelimiter',
              byte: '0',
            );

        expect(response.statusCode, 200);
      });
    });

    group('testEnumParameters', () {
      test('in body data', () async {
        // Not sure if this is correct, we are not sending
        // form data in the body but some weird map
        tester.onGet(
          '/fake',
          (server) => server.reply(200, null),
          data: {
            'enum_form_string': 'formString',
            'enum_form_string_array': Matchers.listParam<String>(
              ListParam(
                ['foo', 'bar'],
                ListFormat.csv,
              ),
            ),
          },
          queryParameters: <String, dynamic>{
            'enum_query_string_array': Matchers.listParam<String>(
              ListParam(
                ['a', 'b', 'c'],
                ListFormat.multi,
              ),
            ),
          },
          headers: <String, dynamic>{
            'content-type': 'application/x-www-form-urlencoded',
          },
        );

        final response = await client.getFakeApi().testEnumParameters(
              enumQueryStringArray: ListBuilder<String>(
                <String>['a', 'b', 'c'],
              ).build(),
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
