import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';
import 'package:dio/dio.dart';
import 'package:openapi/api_util.dart';
import 'package:openapi/model/cat.dart';
import 'package:openapi/serializers.dart';
import 'package:test/test.dart';

void main() {
  group('api_utils', () {
    group('encodeFormParameter should return', () {
      test('empty String for null', () {
        expect(
          encodeFormParameter(
            standardSerializers,
            null,
            const FullType(Cat),
          ),
          '',
        );
      });

      test('String for String', () {
        expect(
          encodeFormParameter(
            standardSerializers,
            'foo',
            const FullType(String),
          ),
          'foo',
        );
      });

      test('List<String> for BuiltList<String>', () {
        expect(
          encodeFormParameter(
            standardSerializers,
            ListBuilder<String>(['foo', 'bar', 'baz']).build(),
            const FullType(BuiltList, [FullType(String)]),
          ),
          ['foo', 'bar', 'baz'],
        );
      });

      test('Map<String, String> for BuiltList<String, String>', () {
        expect(
          encodeFormParameter(
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
          encodeFormParameter(standardSerializers, 0, const FullType(int)),
          0,
        );
        expect(
          encodeFormParameter(standardSerializers, 1, const FullType(int)),
          1,
        );
        expect(
          encodeFormParameter(standardSerializers, 1.0, const FullType(num)),
          1.0,
        );
        expect(
          encodeFormParameter(
              standardSerializers, 1.234, const FullType(double)),
          1.234,
        );
      });

      test('List<num> for BuiltList<num>', () {
        expect(
          encodeFormParameter(
            standardSerializers,
            ListBuilder<num>([0, 1, 2, 3, 4.5, -123.456]).build(),
            const FullType(BuiltList, [FullType(num)]),
          ),
          [0, 1, 2, 3, 4.5, -123.456],
        );
      });

      test('bool for bool', () {
        expect(
          encodeFormParameter(
            standardSerializers,
            true,
            const FullType(bool),
          ),
          true,
        );
        expect(
          encodeFormParameter(
            standardSerializers,
            false,
            const FullType(bool),
          ),
          false,
        );
      });

      test('String for Date', () {
        expect(
          encodeFormParameter(
            standardSerializers,
            DateTime.utc(2020, 8, 11),
            const FullType(DateTime),
          ),
          '2020-08-11T00:00:00.000Z',
        );
      });

      test('String for DateTime', () {
        expect(
          encodeFormParameter(
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
          encodeFormParameter(
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
        'null': encodeFormParameter(
          standardSerializers,
          null,
          const FullType(num),
        ),
        'empty': encodeFormParameter(
          standardSerializers,
          '',
          const FullType(String),
        ),
        'string_list': encodeFormParameter(
          standardSerializers,
          ListBuilder<String>(['foo', 'bar', 'baz']).build(),
          const FullType(BuiltList, [FullType(String)]),
        ),
        'num_list': encodeFormParameter(
          standardSerializers,
          ListBuilder<num>([0, 1, 2, 3, 4.5, -123.456]).build(),
          const FullType(BuiltList, [FullType(num)]),
        ),
        'string_map': encodeFormParameter(
          standardSerializers,
          MapBuilder<String, String>({
            'foo': 'foo-value',
            'bar': 'bar-value',
            'baz': 'baz-value',
          }).build(),
          const FullType(BuiltMap, [FullType(String), FullType(String)]),
        ),
        'bool': encodeFormParameter(
          standardSerializers,
          true,
          const FullType(bool),
        ),
        'double': encodeFormParameter(
          standardSerializers,
          -123.456,
          const FullType(double),
        ),
        'date_time': encodeFormParameter(
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
