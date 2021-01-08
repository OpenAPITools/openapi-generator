//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_client.g.dart';

abstract class ModelClient implements Built<ModelClient, ModelClientBuilder> {

    @nullable
    @BuiltValueField(wireName: r'client')
    String get client;

    ModelClient._();

    static void _initializeBuilder(ModelClientBuilder b) => b;

    factory ModelClient([void updates(ModelClientBuilder b)]) = _$ModelClient;

    @BuiltValueSerializer(custom: true)
    static Serializer<ModelClient> get serializer => _$ModelClientSerializer();
}

class _$ModelClientSerializer implements StructuredSerializer<ModelClient> {

    @override
    final Iterable<Type> types = const [ModelClient, _$ModelClient];
    @override
    final String wireName = r'ModelClient';

    @override
    Iterable<Object> serialize(Serializers serializers, ModelClient object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.client != null) {
            result
                ..add(r'client')
                ..add(serializers.serialize(object.client,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    ModelClient deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ModelClientBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'client':
                    result.client = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
            }
        }
        return result.build();
    }
}

