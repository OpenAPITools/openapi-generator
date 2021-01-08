//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/json_object.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'nullable_class.g.dart';

abstract class NullableClass implements Built<NullableClass, NullableClassBuilder> {

    @nullable
    @BuiltValueField(wireName: r'integer_prop')
    int get integerProp;

    @nullable
    @BuiltValueField(wireName: r'number_prop')
    num get numberProp;

    @nullable
    @BuiltValueField(wireName: r'boolean_prop')
    bool get booleanProp;

    @nullable
    @BuiltValueField(wireName: r'string_prop')
    String get stringProp;

    @nullable
    @BuiltValueField(wireName: r'date_prop')
    DateTime get dateProp;

    @nullable
    @BuiltValueField(wireName: r'datetime_prop')
    DateTime get datetimeProp;

    @nullable
    @BuiltValueField(wireName: r'array_nullable_prop')
    BuiltList<JsonObject> get arrayNullableProp;

    @nullable
    @BuiltValueField(wireName: r'array_and_items_nullable_prop')
    BuiltList<JsonObject> get arrayAndItemsNullableProp;

    @nullable
    @BuiltValueField(wireName: r'array_items_nullable')
    BuiltList<JsonObject> get arrayItemsNullable;

    @nullable
    @BuiltValueField(wireName: r'object_nullable_prop')
    BuiltMap<String, JsonObject> get objectNullableProp;

    @nullable
    @BuiltValueField(wireName: r'object_and_items_nullable_prop')
    BuiltMap<String, JsonObject> get objectAndItemsNullableProp;

    @nullable
    @BuiltValueField(wireName: r'object_items_nullable')
    BuiltMap<String, JsonObject> get objectItemsNullable;

    NullableClass._();

    static void _initializeBuilder(NullableClassBuilder b) => b;

    factory NullableClass([void updates(NullableClassBuilder b)]) = _$NullableClass;

    @BuiltValueSerializer(custom: true)
    static Serializer<NullableClass> get serializer => _$NullableClassSerializer();
}

class _$NullableClassSerializer implements StructuredSerializer<NullableClass> {

    @override
    final Iterable<Type> types = const [NullableClass, _$NullableClass];
    @override
    final String wireName = r'NullableClass';

    @override
    Iterable<Object> serialize(Serializers serializers, NullableClass object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.integerProp != null) {
            result
                ..add(r'integer_prop')
                ..add(serializers.serialize(object.integerProp,
                    specifiedType: const FullType(int)));
        }
        if (object.numberProp != null) {
            result
                ..add(r'number_prop')
                ..add(serializers.serialize(object.numberProp,
                    specifiedType: const FullType(num)));
        }
        if (object.booleanProp != null) {
            result
                ..add(r'boolean_prop')
                ..add(serializers.serialize(object.booleanProp,
                    specifiedType: const FullType(bool)));
        }
        if (object.stringProp != null) {
            result
                ..add(r'string_prop')
                ..add(serializers.serialize(object.stringProp,
                    specifiedType: const FullType(String)));
        }
        if (object.dateProp != null) {
            result
                ..add(r'date_prop')
                ..add(serializers.serialize(object.dateProp,
                    specifiedType: const FullType(DateTime)));
        }
        if (object.datetimeProp != null) {
            result
                ..add(r'datetime_prop')
                ..add(serializers.serialize(object.datetimeProp,
                    specifiedType: const FullType(DateTime)));
        }
        if (object.arrayNullableProp != null) {
            result
                ..add(r'array_nullable_prop')
                ..add(serializers.serialize(object.arrayNullableProp,
                    specifiedType: const FullType(BuiltList, [FullType(JsonObject)])));
        }
        if (object.arrayAndItemsNullableProp != null) {
            result
                ..add(r'array_and_items_nullable_prop')
                ..add(serializers.serialize(object.arrayAndItemsNullableProp,
                    specifiedType: const FullType(BuiltList, [FullType(JsonObject)])));
        }
        if (object.arrayItemsNullable != null) {
            result
                ..add(r'array_items_nullable')
                ..add(serializers.serialize(object.arrayItemsNullable,
                    specifiedType: const FullType(BuiltList, [FullType(JsonObject)])));
        }
        if (object.objectNullableProp != null) {
            result
                ..add(r'object_nullable_prop')
                ..add(serializers.serialize(object.objectNullableProp,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(JsonObject)])));
        }
        if (object.objectAndItemsNullableProp != null) {
            result
                ..add(r'object_and_items_nullable_prop')
                ..add(serializers.serialize(object.objectAndItemsNullableProp,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(JsonObject)])));
        }
        if (object.objectItemsNullable != null) {
            result
                ..add(r'object_items_nullable')
                ..add(serializers.serialize(object.objectItemsNullable,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType(JsonObject)])));
        }
        return result;
    }

    @override
    NullableClass deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = NullableClassBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'integer_prop':
                    result.integerProp = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'number_prop':
                    result.numberProp = serializers.deserialize(value,
                        specifiedType: const FullType(num)) as num;
                    break;
                case r'boolean_prop':
                    result.booleanProp = serializers.deserialize(value,
                        specifiedType: const FullType(bool)) as bool;
                    break;
                case r'string_prop':
                    result.stringProp = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'date_prop':
                    result.dateProp = serializers.deserialize(value,
                        specifiedType: const FullType(DateTime)) as DateTime;
                    break;
                case r'datetime_prop':
                    result.datetimeProp = serializers.deserialize(value,
                        specifiedType: const FullType(DateTime)) as DateTime;
                    break;
                case r'array_nullable_prop':
                    result.arrayNullableProp.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(JsonObject)])) as BuiltList<JsonObject>);
                    break;
                case r'array_and_items_nullable_prop':
                    result.arrayAndItemsNullableProp.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(JsonObject)])) as BuiltList<JsonObject>);
                    break;
                case r'array_items_nullable':
                    result.arrayItemsNullable.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(JsonObject)])) as BuiltList<JsonObject>);
                    break;
                case r'object_nullable_prop':
                    result.objectNullableProp.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(JsonObject)])) as BuiltMap<String, JsonObject>);
                    break;
                case r'object_and_items_nullable_prop':
                    result.objectAndItemsNullableProp.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(JsonObject)])) as BuiltMap<String, JsonObject>);
                    break;
                case r'object_items_nullable':
                    result.objectItemsNullable.replace(serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType(JsonObject)])) as BuiltMap<String, JsonObject>);
                    break;
            }
        }
        return result.build();
    }
}

