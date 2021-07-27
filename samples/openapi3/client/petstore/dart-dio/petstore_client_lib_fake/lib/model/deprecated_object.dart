//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'deprecated_object.g.dart';

abstract class DeprecatedObject implements Built<DeprecatedObject, DeprecatedObjectBuilder> {

    @nullable
    @BuiltValueField(wireName: r'name')
    String get name;

    DeprecatedObject._();

    static void _initializeBuilder(DeprecatedObjectBuilder b) => b;

    factory DeprecatedObject([void updates(DeprecatedObjectBuilder b)]) = _$DeprecatedObject;

    @BuiltValueSerializer(custom: true)
    static Serializer<DeprecatedObject> get serializer => _$DeprecatedObjectSerializer();
}

class _$DeprecatedObjectSerializer implements StructuredSerializer<DeprecatedObject> {

    @override
    final Iterable<Type> types = const [DeprecatedObject, _$DeprecatedObject];
    @override
    final String wireName = r'DeprecatedObject';

    @override
    Iterable<Object> serialize(Serializers serializers, DeprecatedObject object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.name != null) {
            result
                ..add(r'name')
                ..add(serializers.serialize(object.name,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    DeprecatedObject deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = DeprecatedObjectBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'name':
                    result.name = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
            }
        }
        return result.build();
    }
}

