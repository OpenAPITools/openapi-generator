import 'dart:typed_data';

import 'package:built_collection/built_collection.dart';
import 'package:dio/dio.dart';
import 'package:http_mock_adapter/http_mock_adapter.dart';
import 'package:openapi/openapi.dart';
import 'package:test/test.dart';

void main() {
  late Openapi client;
  late DioAdapter tester;

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
            'enum_form_string_array': Matchers.listParam<Object?>(
              ListParam(
                ['foo', 'bar'],
                ListFormat.csv,
              ),
            ),
          },
          queryParameters: <String, dynamic>{
            'enum_query_string': '-efg',
          },
          headers: <String, dynamic>{
            'enum_header_string': '-efg',
            'content-type': 'application/x-www-form-urlencoded',
            'content-length': Matchers.integer,
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

      test('in query parameters', () async {
        tester.onGet(
          '/fake',
          (server) => server.reply(200, null),
          queryParameters: <String, dynamic>{
            'enum_query_string_array': Matchers.listParam<dynamic>(
              ListParam<dynamic>(
                <String>['a', 'b', 'c'],
                ListFormat.multi,
              ),
            ),
            'enum_query_model_array': Matchers.listParam<dynamic>(
              ListParam<dynamic>(
                <String>['_abc', '-efg'],
                ListFormat.multi,
              ),
            ),
            'enum_query_string': 'foo',
            'enum_query_double': 1.23,
            'enum_query_integer': 42,
          },
          headers: <String, dynamic>{
            'enum_header_string': '-efg',
            'content-type': 'application/x-www-form-urlencoded',
          },
          data: <String, dynamic>{},
        );

        final response = await client.getFakeApi().testEnumParameters(
              enumQueryStringArray: ListBuilder<String>(
                <String>['a', 'b', 'c'],
              ).build(),
              enumQueryModelArray: ListBuilder<ModelEnumClass>(
                <ModelEnumClass>[ModelEnumClass.abc, ModelEnumClass.efg],
              ).build(),
              enumQueryString: 'foo',
              enumQueryDouble: 1.23,
              enumQueryInteger: 42,
            );

        expect(response.statusCode, 200);
      });

      test('in header parameters', () async {
        tester.onGet(
          '/fake',
          (server) => server.reply(200, null),
          headers: <String, dynamic>{
            'enum_header_string': 'foo',
            'enum_header_string_array': '[a, b, c]',
            'content-type': 'application/x-www-form-urlencoded',
          },
          data: <String, dynamic>{},
        );

        final response = await client.getFakeApi().testEnumParameters(
              enumHeaderStringArray: ListBuilder<String>(
                <String>['a', 'b', 'c'],
              ).build(),
              enumHeaderString: 'foo',
            );

        expect(response.statusCode, 200);
      });
    });
  });
}
