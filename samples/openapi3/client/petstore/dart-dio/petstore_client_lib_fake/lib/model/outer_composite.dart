//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'outer_composite.g.dart';

abstract class OuterComposite implements Built<OuterComposite, OuterCompositeBuilder> {

    @nullable
    @BuiltValueField(wireName: r'my_number')
    num get myNumber;

    @nullable
    @BuiltValueField(wireName: r'my_string')
    String get myString;

    @nullable
    @BuiltValueField(wireName: r'my_boolean')
    bool get myBoolean;

    OuterComposite._();

    static void _initializeBuilder(OuterCompositeBuilder b) => b;

    factory OuterComposite([void updates(OuterCompositeBuilder b)]) = _$OuterComposite;

    @BuiltValueSerializer(custom: true)
    static Serializer<OuterComposite> get serializer => _$OuterCompositeSerializer();
}

class _$OuterCompositeSerializer implements StructuredSerializer<OuterComposite> {

    @override
    final Iterable<Type> types = const [OuterComposite, _$OuterComposite];
    @override
    final String wireName = r'OuterComposite';

    @override
    Iterable<Object> serialize(Serializers serializers, OuterComposite object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.myNumber != null) {
            result
                ..add(r'my_number')
                ..add(serializers.serialize(object.myNumber,
                    specifiedType: const FullType(num)));
        }
        if (object.myString != null) {
            result
                ..add(r'my_string')
                ..add(serializers.serialize(object.myString,
                    specifiedType: const FullType(String)));
        }
        if (object.myBoolean != null) {
            result
                ..add(r'my_boolean')
                ..add(serializers.serialize(object.myBoolean,
                    specifiedType: const FullType(bool)));
        }
        return result;
    }

    @override
    OuterComposite deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = OuterCompositeBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'my_number':
                    result.myNumber = serializers.deserialize(value,
                        specifiedType: const FullType(num)) as num;
                    break;
                case r'my_string':
                    result.myString = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'my_boolean':
                    result.myBoolean = serializers.deserialize(value,
                        specifiedType: const FullType(bool)) as bool;
                    break;
            }
        }
        return result.build();
    }
}

