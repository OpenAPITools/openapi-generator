//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:openapi/model/animal.dart';
import 'package:openapi/model/cat_all_of.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'cat.g.dart';

abstract class Cat implements Built<Cat, CatBuilder> {

    @BuiltValueField(wireName: r'className')
    String get className;

    @BuiltValueField(wireName: r'color')
    String get color;

    @nullable
    @BuiltValueField(wireName: r'declawed')
    bool get declawed;

    Cat._();

    static void _initializeBuilder(CatBuilder b) => b
        ..color = 'red';

    factory Cat([void updates(CatBuilder b)]) = _$Cat;

    @BuiltValueSerializer(custom: true)
    static Serializer<Cat> get serializer => _$CatSerializer();
}

class _$CatSerializer implements StructuredSerializer<Cat> {

    @override
    final Iterable<Type> types = const [Cat, _$Cat];
    @override
    final String wireName = r'Cat';

    @override
    Iterable<Object> serialize(Serializers serializers, Cat object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        result
            ..add(r'className')
            ..add(serializers.serialize(object.className,
                specifiedType: const FullType(String)));
        if (object.color != null) {
            result
                ..add(r'color')
                ..add(serializers.serialize(object.color,
                    specifiedType: const FullType(String)));
        }
        if (object.declawed != null) {
            result
                ..add(r'declawed')
                ..add(serializers.serialize(object.declawed,
                    specifiedType: const FullType(bool)));
        }
        return result;
    }

    @override
    Cat deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = CatBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'className':
                    result.className = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'color':
                    result.color = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'declawed':
                    result.declawed = serializers.deserialize(value,
                        specifiedType: const FullType(bool)) as bool;
                    break;
            }
        }
        return result.build();
    }
}

