//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_of_array_of_number_only.g.dart';

abstract class ArrayOfArrayOfNumberOnly implements Built<ArrayOfArrayOfNumberOnly, ArrayOfArrayOfNumberOnlyBuilder> {

    @nullable
    @BuiltValueField(wireName: r'ArrayArrayNumber')
    BuiltList<BuiltList<num>> get arrayArrayNumber;

    ArrayOfArrayOfNumberOnly._();

    static void _initializeBuilder(ArrayOfArrayOfNumberOnlyBuilder b) => b;

    factory ArrayOfArrayOfNumberOnly([void updates(ArrayOfArrayOfNumberOnlyBuilder b)]) = _$ArrayOfArrayOfNumberOnly;

    @BuiltValueSerializer(custom: true)
    static Serializer<ArrayOfArrayOfNumberOnly> get serializer => _$ArrayOfArrayOfNumberOnlySerializer();
}

class _$ArrayOfArrayOfNumberOnlySerializer implements StructuredSerializer<ArrayOfArrayOfNumberOnly> {

    @override
    final Iterable<Type> types = const [ArrayOfArrayOfNumberOnly, _$ArrayOfArrayOfNumberOnly];
    @override
    final String wireName = r'ArrayOfArrayOfNumberOnly';

    @override
    Iterable<Object> serialize(Serializers serializers, ArrayOfArrayOfNumberOnly object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.arrayArrayNumber != null) {
            result
                ..add(r'ArrayArrayNumber')
                ..add(serializers.serialize(object.arrayArrayNumber,
                    specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(num)])])));
        }
        return result;
    }

    @override
    ArrayOfArrayOfNumberOnly deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ArrayOfArrayOfNumberOnlyBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'ArrayArrayNumber':
                    result.arrayArrayNumber.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(num)])])) as BuiltList<BuiltList<num>>);
                    break;
            }
        }
        return result.build();
    }
}

