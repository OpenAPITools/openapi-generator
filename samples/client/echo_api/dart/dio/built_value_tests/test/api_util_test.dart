import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';
import 'package:dio/dio.dart';
import 'package:openapi/openapi.dart';
import 'package:openapi/src/api_util.dart';
import 'package:test/test.dart';

void main() {
  group('api_utils', () {
    final repository = BuiltValueJsonRepository(standardSerializers);
    group('encodeQueryParameter should encode', () {
      group('String enum', () {
        test('empty String for null', () {
          expect(
            encodeQueryParameter(
              repository,
              null,
              const TypeInfo(ModelEnumClass),
            ),
            '',
          );
        });

        test('correct String for value', () {
          expect(
            encodeQueryParameter(
              repository,
              ModelEnumClass.leftParenthesisXyzRightParenthesis,
              const TypeInfo(ModelEnumClass),
            ),
            '(xyz)',
          );
        });
      });

      group('int enum', () {
        test('empty String for null', () {
          expect(
            encodeQueryParameter(
              repository,
              null,
              const TypeInfo(EnumTestEnumIntegerEnum),
            ),
            '',
          );
        });

        test('correct int for value', () {
          expect(
            encodeQueryParameter(
              repository,
              EnumTestEnumIntegerEnum.numberNegative1,
              const TypeInfo(EnumTestEnumIntegerEnum),
            ),
            -1,
          );
        });
      });
    });

    group('encodeCollectionQueryParameter should encode', () {
      final serializers = (standardSerializers.toBuilder()
            ..addBuilderFactory(
              const FullType(BuiltList, [FullType(ModelEnumClass)]),
              () => ListBuilder<ModelEnumClass>(),
            )
            ..addBuilderFactory(
              const FullType(BuiltList, [FullType(EnumTestEnumIntegerEnum)]),
              () => ListBuilder<EnumTestEnumIntegerEnum>(),
            ))
          .build();
      final repository = BuiltValueJsonRepository(serializers);
      group('String enum', () {
        test('empty ListParam for empty list', () {
          final param = encodeCollectionQueryParameter(
            repository,
            <ModelEnumClass>[].build(),
            const TypeInfo(BuiltList, [TypeInfo(ModelEnumClass)]),
          );

          expect(param.value, isEmpty);
          expect(param.format, ListFormat.multi);
        });

        test('correct ListParam for value', () {
          final param = encodeCollectionQueryParameter(
            repository,
            <ModelEnumClass>[
              ModelEnumClass.leftParenthesisXyzRightParenthesis,
              ModelEnumClass.efg,
            ].build(),
            const TypeInfo(BuiltList, [TypeInfo(ModelEnumClass)]),
          );

          expect(param.value, hasLength(2));
          expect(param.value, containsAll(['-efg', '(xyz)']));
          expect(param.format, ListFormat.multi);
        });
      });

      group('int enum', () {
        test('empty ListParam for empty list', () {
          final param = encodeCollectionQueryParameter(
            repository,
            <EnumTestEnumIntegerEnum>[].build(),
            const TypeInfo(BuiltList, [TypeInfo(EnumTestEnumIntegerEnum)]),
          );

          expect(param.value, isEmpty);
          expect(param.format, ListFormat.multi);
        });

        test('correct ListParam for value', () {
          final param = encodeCollectionQueryParameter(
            repository,
            <EnumTestEnumIntegerEnum>[
              EnumTestEnumIntegerEnum.number1,
              EnumTestEnumIntegerEnum.numberNegative1,
            ].build(),
            const TypeInfo(BuiltList, [TypeInfo(EnumTestEnumIntegerEnum)]),
          );

          expect(param.value, hasLength(2));
          expect(param.value, containsAll([1, -1]));
          expect(param.format, ListFormat.multi);
        });
      });
    });

    group('encodeFormParameter should return', () {
      test('empty String for null', () {
        expect(
          encodeFormParameter(
            repository,
            null,
            const TypeInfo(Cat),
          ),
          '',
        );
      });

      test('String for String', () {
        expect(
          encodeFormParameter(
            repository,
            'foo',
            const TypeInfo(String),
          ),
          'foo',
        );
      });

      test('List<String> for BuiltList<String>', () {
        expect(
          encodeFormParameter(
            repository,
            ListBuilder<String>(['foo', 'bar', 'baz']).build(),
            const TypeInfo(BuiltList, [TypeInfo(String)]),
          ),
          ['foo', 'bar', 'baz'],
        );
      });

      test('Map<String, String> for BuiltList<String, String>', () {
        expect(
          encodeFormParameter(
            repository,
            MapBuilder<String, String>({
              'foo': 'foo-value',
              'bar': 'bar-value',
              'baz': 'baz-value',
            }).build(),
            const TypeInfo(BuiltMap, [TypeInfo(String), TypeInfo(String)]),
          ),
          {
            'foo': 'foo-value',
            'bar': 'bar-value',
            'baz': 'baz-value',
          },
        );
      });

      test('num for num', () {
        expect(
          encodeFormParameter(repository, 0, const TypeInfo(int)),
          0,
        );
        expect(
          encodeFormParameter(repository, 1, const TypeInfo(int)),
          1,
        );
        expect(
          encodeFormParameter(repository, 1.0, const TypeInfo(num)),
          1.0,
        );
        expect(
          encodeFormParameter(repository, 1.234, const TypeInfo(double)),
          1.234,
        );
      });

      test('List<num> for BuiltList<num>', () {
        expect(
          encodeFormParameter(
            repository,
            ListBuilder<num>([0, 1, 2, 3, 4.5, -123.456]).build(),
            const TypeInfo(BuiltList, [TypeInfo(num)]),
          ),
          [0, 1, 2, 3, 4.5, -123.456],
        );
      });

      test('bool for bool', () {
        expect(
          encodeFormParameter(
            repository,
            true,
            const TypeInfo(bool),
          ),
          true,
        );
        expect(
          encodeFormParameter(
            repository,
            false,
            const TypeInfo(bool),
          ),
          false,
        );
      });

      test('String for Date', () {
        expect(
          encodeFormParameter(
            repository,
            DateTime.utc(2020, 8, 11),
            const TypeInfo(DateTime),
          ),
          '2020-08-11T00:00:00.000Z',
        );
      });

      test('String for DateTime', () {
        expect(
          encodeFormParameter(
            repository,
            DateTime.utc(2020, 8, 11, 12, 30, 55, 123),
            const TypeInfo(DateTime),
          ),
          '2020-08-11T12:30:55.123Z',
        );
      });

      test('JSON String for Cat', () {
        // Not sure that is even a valid case,
        // sending complex objects via FormData may not work as expected

        expect(
          encodeFormParameter(
            repository,
            (CatBuilder()
                  ..color = 'black'
                  ..className = 'cat'
                  ..declawed = false)
                .build(),
            const TypeInfo(Cat),
          ),
          equals({
            "className": "cat",
            "color": "black",
            "declawed": false,
          }),
        );
      });
    });

    test('encodes FormData correctly', () {
      final data = FormData.fromMap({
        'null': encodeFormParameter(
          repository,
          null,
          const TypeInfo(num),
        ),
        'empty': encodeFormParameter(
          repository,
          '',
          const TypeInfo(String),
        ),
        'string_list': encodeFormParameter(
          repository,
          ListBuilder<String>(['foo', 'bar', 'baz']).build(),
          const TypeInfo(BuiltList, [TypeInfo(String)]),
        ),
        'num_list': encodeFormParameter(
          repository,
          ListBuilder<num>([0, 1, 2, 3, 4.5, -123.456]).build(),
          const TypeInfo(BuiltList, [TypeInfo(num)]),
        ),
        'string_map': encodeFormParameter(
          repository,
          MapBuilder<String, String>({
            'foo': 'foo-value',
            'bar': 'bar-value',
            'baz': 'baz-value',
          }).build(),
          const TypeInfo(BuiltMap, [TypeInfo(String), TypeInfo(String)]),
        ),
        'bool': encodeFormParameter(
          repository,
          true,
          const TypeInfo(bool),
        ),
        'double': encodeFormParameter(
          repository,
          -123.456,
          const TypeInfo(double),
        ),
        'date_time': encodeFormParameter(
          repository,
          DateTime.utc(2020, 8, 11, 12, 30, 55, 123),
          const TypeInfo(DateTime),
        ),
      });

      expect(
        data.fields,
        pairwiseCompare<MapEntry<String, String>, MapEntry<String, String>>(
          <MapEntry<String, String>>[
            MapEntry('null', ''),
            MapEntry('empty', ''),
            MapEntry('string_list', 'foo'),
            MapEntry('string_list', 'bar'),
            MapEntry('string_list', 'baz'),
            MapEntry('num_list', '0'),
            MapEntry('num_list', '1'),
            MapEntry('num_list', '2'),
            MapEntry('num_list', '3'),
            MapEntry('num_list', '4.5'),
            MapEntry('num_list', '-123.456'),
            MapEntry('string_map[foo]', 'foo-value'),
            MapEntry('string_map[bar]', 'bar-value'),
            MapEntry('string_map[baz]', 'baz-value'),
            MapEntry('bool', 'true'),
            MapEntry('double', '-123.456'),
            MapEntry('date_time', '2020-08-11T12:30:55.123Z'),
          ],
          (e, a) => e.key == a.key && e.value == a.value,
          'Compares map entries by key and value',
        ),
      );
    });
  });
}
