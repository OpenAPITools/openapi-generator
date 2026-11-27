import 'package:dio/dio.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/src/model/outer_enum.dart';
import 'package:test/test.dart';

void main() {
  group('path parameter enum encoding', () {
    test('uses the wire value when it differs from the Dart enum name', () {
      const value = OuterEnum.unknownDefaultOpenApi;

      expect(value.name, 'unknownDefaultOpenApi');
      expect(value.toString(), 'unknown_default_open_api');
      expect(encodePathParameter(value), 'unknown_default_open_api');
    });

    test('uses wire values for enum collection elements', () {
      expect(
        encodePathParameter(const <OuterEnum>[
          OuterEnum.unknownDefaultOpenApi,
          OuterEnum.placed,
        ], format: ListFormat.csv),
        'unknown_default_open_api,placed',
      );
    });
  });
}
