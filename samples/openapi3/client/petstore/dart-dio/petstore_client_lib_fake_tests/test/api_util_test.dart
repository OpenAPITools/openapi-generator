import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';
import 'package:dio/dio.dart';
import 'package:openapi/api_util.dart';
import 'package:openapi/model/cat.dart';
import 'package:openapi/serializers.dart';
import 'package:test/test.dart';

void main() {
  group('api_utils', () {
    group('parameterToString should return', () {
      test('empty String for null', () {
        expect(
          parameterToString(
            standardSerializers,
            null,
            const FullType(Cat),
          ),
          '',
        );
      });

      test('String for String', () {
        expect(
          parameterToString(
            standardSerializers,
            'foo',
            const FullType(String),
          ),
          'foo',
        );
      });

      test('List<String> for BuiltList<String>', () {
        expect(
          parameterToString(
            standardSerializers,
            ListBuilder<String>(['foo', 'bar', 'baz']).build(),
            const FullType(BuiltList, [FullType(String)]),
          ),
          ['foo', 'bar', 'baz'],
        );
      });

      test('Map<String, String> for BuiltList<String, String>', () {
        expect(
          parameterToString(
            standardSerializers,
            MapBuilder<String, String>({
              'foo': 'foo-value',
              'bar': 'bar-value',
              'baz': 'baz-value',
            }).build(),
            const FullType(BuiltMap, [FullType(String), FullType(String)]),
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
          parameterToString(standardSerializers, 0, const FullType(int)),
          0,
        );
        expect(
          parameterToString(standardSerializers, 1, const FullType(int)),
          1,
        );
        expect(
          parameterToString(standardSerializers, 1.0, const FullType(num)),
          1.0,
        );
        expect(
          parameterToString(standardSerializers, 1.234, const FullType(double)),
          1.234,
        );
      });

      test('List<num> for BuiltList<num>', () {
        expect(
          parameterToString(
            standardSerializers,
            ListBuilder<num>([0, 1, 2, 3, 4.5, -123.456]).build(),
            const FullType(BuiltList, [FullType(num)]),
          ),
          [0, 1, 2, 3, 4.5, -123.456],
        );
      });

      test('bool for bool', () {
        expect(
          parameterToString(
            standardSerializers,
            true,
            const FullType(bool),
          ),
          true,
        );
        expect(
          parameterToString(
            standardSerializers,
            false,
            const FullType(bool),
          ),
          false,
        );
      });

      test('String for Date', () {
        expect(
          parameterToString(
            standardSerializers,
            DateTime.utc(2020, 8, 11),
            const FullType(DateTime),
          ),
          '2020-08-11T00:00:00.000Z',
        );
      });

      test('String for DateTime', () {
        expect(
          parameterToString(
            standardSerializers,
            DateTime.utc(2020, 8, 11, 12, 30, 55, 123),
            const FullType(DateTime),
          ),
          '2020-08-11T12:30:55.123Z',
        );
      });

      test('JSON String for Cat', () {
        // Not sure that is even a valid case,
        // sending complex objects via FormData may not work as expected
        expect(
          parameterToString(
            standardSerializers,
            (CatBuilder()
                  ..color = 'black'
                  ..className = 'cat'
                  ..declawed = false)
                .build(),
            const FullType(Cat),
          ),
          '{"className":"cat","color":"black","declawed":false}',
        );
      });
    });

    test('encodes FormData correctly', () {
      final data = FormData.fromMap({
        'null': parameterToString(
          standardSerializers,
          null,
          const FullType(num),
        ),
        'empty': parameterToString(
          standardSerializers,
          '',
          const FullType(String),
        ),
        'string_list': parameterToString(
          standardSerializers,
          ListBuilder<String>(['foo', 'bar', 'baz']).build(),
          const FullType(BuiltList, [FullType(String)]),
        ),
        'num_list': parameterToString(
          standardSerializers,
          ListBuilder<num>([0, 1, 2, 3, 4.5, -123.456]).build(),
          const FullType(BuiltList, [FullType(num)]),
        ),
        'string_map': parameterToString(
          standardSerializers,
          MapBuilder<String, String>({
            'foo': 'foo-value',
            'bar': 'bar-value',
            'baz': 'baz-value',
          }).build(),
          const FullType(BuiltMap, [FullType(String), FullType(String)]),
        ),
        'bool': parameterToString(
          standardSerializers,
          true,
          const FullType(bool),
        ),
        'double': parameterToString(
          standardSerializers,
          -123.456,
          const FullType(double),
        ),
        'date_time': parameterToString(
          standardSerializers,
          DateTime.utc(2020, 8, 11, 12, 30, 55, 123),
          const FullType(DateTime),
        ),
      });

      expect(
        data.fields,
        pairwiseCompare<MapEntry<String, String>, MapEntry<String, String>>(
          <MapEntry<String, String>>[
            MapEntry('null', ''),
            MapEntry('empty', ''),
            MapEntry('string_list[]', 'foo'),
            MapEntry('string_list[]', 'bar'),
            MapEntry('string_list[]', 'baz'),
            MapEntry('num_list[]', '0'),
            MapEntry('num_list[]', '1'),
            MapEntry('num_list[]', '2'),
            MapEntry('num_list[]', '3'),
            MapEntry('num_list[]', '4.5'),
            MapEntry('num_list[]', '-123.456'),
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
