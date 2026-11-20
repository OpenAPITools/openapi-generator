import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';
import 'package:dio/dio.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/src/model/outer_enum.dart';
import 'package:openapi/src/serializers.dart';
import 'package:test/test.dart';

void main() {
  group('path parameter enum encoding', () {
    test('uses the serialized wire value instead of the EnumClass name', () {
      const value = OuterEnum.unknownDefaultOpenApi;

      expect(value.name, 'unknownDefaultOpenApi');
      expect(
        encodePathParameter(
          standardSerializers,
          value,
          const FullType(OuterEnum),
        ),
        'unknown_default_open_api',
      );
    });

    test('uses serialized wire values for enum collection elements', () {
      final serializers =
          (standardSerializers.toBuilder()..addBuilderFactory(
                const FullType(BuiltList, [FullType(OuterEnum)]),
                () => ListBuilder<OuterEnum>(),
              ))
              .build();

      expect(
        encodePathParameter(
          serializers,
          BuiltList<OuterEnum>(const <OuterEnum>[
            OuterEnum.unknownDefaultOpenApi,
            OuterEnum.placed,
          ]),
          const FullType(BuiltList, [FullType(OuterEnum)]),
          format: ListFormat.csv,
        ),
        'unknown_default_open_api,placed',
      );
    });
  });
}
