import 'dart:convert';

import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';
import 'package:dio/dio.dart';
import 'package:openapi/src/api_util.dart';
import 'package:openapi/src/model/category.dart';
import 'package:openapi/src/serializers.dart';
import 'package:test/test.dart';

void main() {
  group('model request encoding', () {
    final model = Category((b) => b..name = 'Milo & friends');
    const modelType = FullType(Category);

    Future<String> transform(Object? pet, String contentType) {
      return SyncTransformer().transformRequest(RequestOptions(
        path: '/pets',
        method: 'POST',
        contentType: contentType,
        data: <String, dynamic>{'pet': pet},
      ));
    }

    test('JSON request preserves the model as an object and round-trips',
        () async {
      // Matches the generated JSON-body branch: serialize first, then let
      // Dio encode the complete body once.
      final serialized = standardSerializers.serialize(
        model,
        specifiedType: modelType,
      );
      final body = await transform(serialized, Headers.jsonContentType);
      final decoded = jsonDecode(body) as Map<String, dynamic>;

      expect(decoded['pet'], isA<Map<String, dynamic>>());
      expect(decoded['pet'], <String, dynamic>{'name': 'Milo & friends'});
      expect(
        standardSerializers.deserialize(decoded['pet'],
            specifiedType: modelType),
        model,
      );
    });

    test('default URL-encoded model field is JSON and round-trips', () async {
      // OpenAPI 3.0.4 section 4.7.15.2.1: when no style/explode/allowReserved
      // is explicitly set, an object field uses application/json encoding.
      // Matches the generated non-container form-parameter call site.
      final encoded = encodeParameter<Category>(
        standardSerializers,
        model,
        modelType,
        asString: true,
      );
      final body = await transform(encoded, Headers.formUrlEncodedContentType);
      final fields = Uri.splitQueryString(body);

      expect(fields.keys, <String>['pet']);
      final decodedPet = jsonDecode(fields['pet']!);
      expect(decodedPet, <String, dynamic>{'name': 'Milo & friends'});
      expect(
        standardSerializers.deserialize(decodedPet, specifiedType: modelType),
        model,
      );
    });

    test('query and multipart model values remain serialized objects', () {
      for (final forMultipart in <bool>[false, true]) {
        final encoded = encodeParameter<Category>(
          standardSerializers,
          model,
          modelType,
          forMultipart: forMultipart,
        );
        expect(encoded, <String, dynamic>{'name': 'Milo & friends'});
      }
    });

    test('serialized strings are not JSON-quoted in form fields', () {
      final date = DateTime.utc(2026, 10, 12);
      expect(
        encodeParameter<DateTime>(
          standardSerializers,
          date,
          const FullType(DateTime),
          asString: true,
        ),
        '2026-10-12T00:00:00.000Z',
      );
    });

    test('null parameters are encoded as empty values', () {
      expect(
        encodeParameter<String>(
          standardSerializers,
          null,
          const FullType(String),
        ),
        '',
      );
    });

    test('explicit maps and list formats retain their existing behavior', () {
      final serializers = (standardSerializers.toBuilder()
            ..addBuilderFactory(
              const FullType(BuiltList, [FullType(String)]),
              () => ListBuilder<String>(),
            )
            ..addBuilderFactory(
              const FullType(BuiltMap, [FullType(String), FullType(String)]),
              () => MapBuilder<String, String>(),
            ))
          .build();
      final map = BuiltMap<String, String>(<String, String>{'name': 'Milo'});
      expect(
        encodeParameter<String>(
          serializers,
          map,
          const FullType(BuiltMap, [FullType(String), FullType(String)]),
          asString: true,
        ),
        <String, String>{'name': 'Milo'},
      );

      final list = BuiltList<String>(<String>['Milo', 'friends']);
      for (final format in <ListFormat>[ListFormat.multi, ListFormat.csv]) {
        final encoded = encodeParameter<String>(
          serializers,
          list,
          const FullType(BuiltList, [FullType(String)]),
          asString: true,
          format: format,
        );
        if (format == ListFormat.csv) {
          expect(encoded, 'Milo,friends');
        } else {
          expect(encoded, isA<ListParam<Object?>>());
          final param = encoded as ListParam<Object?>;
          expect(param.value, <String>['Milo', 'friends']);
          expect(param.format, ListFormat.multi);
        }
      }
    });

    test('nullable-item query lists preserve standard collection formats', () {
      const type = FullType(BuiltList, [FullType.nullable(String)]);
      final serializers =
          (standardSerializers.toBuilder()
                ..addBuilderFactory(type, () => ListBuilder<String?>()))
              .build();
      final values = BuiltList<String?>(<String?>['blue', null, 'brown']);
      final expected = <ListFormat, String>{
        ListFormat.multi: 'color=blue&color=brown',
        ListFormat.csv: 'color=blue,brown',
        ListFormat.ssv: 'color=blue%20brown',
        ListFormat.pipes: 'color=blue|brown',
      };

      for (final entry in expected.entries) {
        final encoded = encodeParameter<String?>(
          serializers,
          values,
          type,
          format: entry.key,
          omitNullCollectionItems: true,
        );

        expect(encoded, isA<ListParam<Object?>>());
        expect(
          Transformer.urlEncodeQueryMap(<String, dynamic>{'color': encoded}),
          entry.value,
        );
      }
    });

    test('query null filtering handles edge positions and all-null lists', () {
      const type = FullType(BuiltList, [FullType.nullable(String)]);
      final serializers =
          (standardSerializers.toBuilder()
                ..addBuilderFactory(type, () => ListBuilder<String?>()))
              .build();
      final cases = <BuiltList<String?>, String>{
        BuiltList<String?>(<String?>[null, 'blue', null, 'brown', null]):
            'color=blue&color=brown',
        BuiltList<String?>(<String?>[null, null]): '',
        BuiltList<String?>(<String?>['', null, 'brown']): 'color=&color=brown',
      };

      for (final entry in cases.entries) {
        final encoded = encodeParameter<String?>(
          serializers,
          entry.key,
          type,
          omitNullCollectionItems: true,
        );

        expect(
          Transformer.urlEncodeQueryMap(<String, dynamic>{'color': encoded}),
          entry.value,
        );
      }
    });

    test('nullable query items encode reserved data before delimiters', () {
      const type = FullType(BuiltList, [FullType.nullable(String)]);
      final serializers =
          (standardSerializers.toBuilder()
                ..addBuilderFactory(type, () => ListBuilder<String?>()))
              .build();
      final values = BuiltList<String?>(<String?>[
        'blue,green',
        null,
        'black & white',
      ]);
      final expected = <ListFormat, String>{
        ListFormat.multi: 'color=blue%2Cgreen&color=black+%26+white',
        ListFormat.csv: 'color=blue%2Cgreen,black+%26+white',
        ListFormat.ssv: 'color=blue%2Cgreen%20black+%26+white',
        ListFormat.pipes: 'color=blue%2Cgreen|black+%26+white',
      };

      for (final entry in expected.entries) {
        final encoded = encodeParameter<String?>(
          serializers,
          values,
          type,
          format: entry.key,
          omitNullCollectionItems: true,
        );

        expect(
          Transformer.urlEncodeQueryMap(<String, dynamic>{'color': encoded}),
          entry.value,
        );
      }
    });

    test('query null filtering is opt-in and does not mutate its input', () {
      const type = FullType(BuiltList, [FullType.nullable(String)]);
      final serializers =
          (standardSerializers.toBuilder()
                ..addBuilderFactory(type, () => ListBuilder<String?>()))
              .build();
      final values = BuiltList<String?>(<String?>['blue', null, 'brown']);

      final unfiltered = encodeParameter<String?>(
        serializers,
        values,
        type,
      ) as ListParam<Object?>;
      final filtered = encodeParameter<String?>(
        serializers,
        values,
        type,
        omitNullCollectionItems: true,
      ) as ListParam<Object?>;

      expect(unfiltered.value, <Object?>['blue', null, 'brown']);
      expect(filtered.value, <Object?>['blue', 'brown']);
      expect(values.toList(), <String?>['blue', null, 'brown']);
    });

    test('explicit JSON form field needs a second decode to recover the model',
        () async {
      // Models a server contract explicitly requiring JSON text in `pet`.
      // This conversion belongs at that form call site, not in JSON bodies.
      final serialized = standardSerializers.serialize(
        model,
        specifiedType: modelType,
      );
      final body = await transform(
        jsonEncode(serialized),
        Headers.formUrlEncodedContentType,
      );
      final fields = Uri.splitQueryString(body);

      expect(fields.keys, <String>['pet']);
      expect(fields['pet'], isA<String>());
      final decodedPet = jsonDecode(fields['pet']!);
      expect(decodedPet, <String, dynamic>{'name': 'Milo & friends'});
      expect(
        standardSerializers.deserialize(decodedPet, specifiedType: modelType),
        model,
      );

      // Using the same string inside a JSON body does not recover an object:
      // the server would receive a string property instead.
      final jsonBody = await transform(fields['pet'], Headers.jsonContentType);
      final decodedJson = jsonDecode(jsonBody) as Map<String, dynamic>;
      expect(decodedJson['pet'], isA<String>());
    });
  });
}
