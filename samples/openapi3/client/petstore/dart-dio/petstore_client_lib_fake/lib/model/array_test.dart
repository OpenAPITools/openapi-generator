//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:openapi/model/read_only_first.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_test.g.dart';

abstract class ArrayTest implements Built<ArrayTest, ArrayTestBuilder> {

    @nullable
    @BuiltValueField(wireName: r'array_of_string')
    BuiltList<String> get arrayOfString;

    @nullable
    @BuiltValueField(wireName: r'array_array_of_integer')
    BuiltList<BuiltList<int>> get arrayArrayOfInteger;

    @nullable
    @BuiltValueField(wireName: r'array_array_of_model')
    BuiltList<BuiltList<ReadOnlyFirst>> get arrayArrayOfModel;

    ArrayTest._();

    static void _initializeBuilder(ArrayTestBuilder b) => b;

    factory ArrayTest([void updates(ArrayTestBuilder b)]) = _$ArrayTest;

    @BuiltValueSerializer(custom: true)
    static Serializer<ArrayTest> get serializer => _$ArrayTestSerializer();
}

class _$ArrayTestSerializer implements StructuredSerializer<ArrayTest> {

    @override
    final Iterable<Type> types = const [ArrayTest, _$ArrayTest];
    @override
    final String wireName = r'ArrayTest';

    @override
    Iterable<Object> serialize(Serializers serializers, ArrayTest object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.arrayOfString != null) {
            result
                ..add(r'array_of_string')
                ..add(serializers.serialize(object.arrayOfString,
                    specifiedType: const FullType(BuiltList, [FullType(String)])));
        }
        if (object.arrayArrayOfInteger != null) {
            result
                ..add(r'array_array_of_integer')
                ..add(serializers.serialize(object.arrayArrayOfInteger,
                    specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(int)])])));
        }
        if (object.arrayArrayOfModel != null) {
            result
                ..add(r'array_array_of_model')
                ..add(serializers.serialize(object.arrayArrayOfModel,
                    specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(ReadOnlyFirst)])])));
        }
        return result;
    }

    @override
    ArrayTest deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ArrayTestBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'array_of_string':
                    result.arrayOfString.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(String)])) as BuiltList<String>);
                    break;
                case r'array_array_of_integer':
                    result.arrayArrayOfInteger.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(int)])])) as BuiltList<BuiltList<int>>);
                    break;
                case r'array_array_of_model':
                    result.arrayArrayOfModel.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(ReadOnlyFirst)])])) as BuiltList<BuiltList<ReadOnlyFirst>>);
                    break;
            }
        }
        return result.build();
    }
}

