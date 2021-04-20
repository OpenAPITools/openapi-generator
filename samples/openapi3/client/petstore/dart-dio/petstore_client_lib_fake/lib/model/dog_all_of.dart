//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'dog_all_of.g.dart';

abstract class DogAllOf implements Built<DogAllOf, DogAllOfBuilder> {

    @nullable
    @BuiltValueField(wireName: r'breed')
    String get breed;

    DogAllOf._();

    static void _initializeBuilder(DogAllOfBuilder b) => b;

    factory DogAllOf([void updates(DogAllOfBuilder b)]) = _$DogAllOf;

    @BuiltValueSerializer(custom: true)
    static Serializer<DogAllOf> get serializer => _$DogAllOfSerializer();
}

class _$DogAllOfSerializer implements StructuredSerializer<DogAllOf> {

    @override
    final Iterable<Type> types = const [DogAllOf, _$DogAllOf];
    @override
    final String wireName = r'DogAllOf';

    @override
    Iterable<Object> serialize(Serializers serializers, DogAllOf object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.breed != null) {
            result
                ..add(r'breed')
                ..add(serializers.serialize(object.breed,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    DogAllOf deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = DogAllOfBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'breed':
                    result.breed = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
            }
        }
        return result.build();
    }
}

