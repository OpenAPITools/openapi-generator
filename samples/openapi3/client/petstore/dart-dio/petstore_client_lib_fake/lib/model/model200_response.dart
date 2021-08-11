//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model200_response.g.dart';

abstract class Model200Response implements Built<Model200Response, Model200ResponseBuilder> {

    @nullable
    @BuiltValueField(wireName: r'name')
    int get name;

    @nullable
    @BuiltValueField(wireName: r'class')
    String get class_;

    Model200Response._();

    static void _initializeBuilder(Model200ResponseBuilder b) => b;

    factory Model200Response([void updates(Model200ResponseBuilder b)]) = _$Model200Response;

    @BuiltValueSerializer(custom: true)
    static Serializer<Model200Response> get serializer => _$Model200ResponseSerializer();
}

class _$Model200ResponseSerializer implements StructuredSerializer<Model200Response> {

    @override
    final Iterable<Type> types = const [Model200Response, _$Model200Response];
    @override
    final String wireName = r'Model200Response';

    @override
    Iterable<Object> serialize(Serializers serializers, Model200Response object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.name != null) {
            result
                ..add(r'name')
                ..add(serializers.serialize(object.name,
                    specifiedType: const FullType(int)));
        }
        if (object.class_ != null) {
            result
                ..add(r'class')
                ..add(serializers.serialize(object.class_,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    Model200Response deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = Model200ResponseBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'name':
                    result.name = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'class':
                    result.class_ = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
            }
        }
        return result.build();
    }
}

