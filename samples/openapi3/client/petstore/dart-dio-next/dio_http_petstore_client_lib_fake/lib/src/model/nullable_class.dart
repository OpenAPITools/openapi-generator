//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/date.dart';
import 'package:built_value/json_object.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'nullable_class.g.dart';

/// NullableClass
///
/// Properties:
/// * [integerProp] 
/// * [numberProp] 
/// * [booleanProp] 
/// * [stringProp] 
/// * [dateProp] 
/// * [datetimeProp] 
/// * [arrayNullableProp] 
/// * [arrayAndItemsNullableProp] 
/// * [arrayItemsNullable] 
/// * [objectNullableProp] 
/// * [objectAndItemsNullableProp] 
/// * [objectItemsNullable] 
abstract class NullableClass implements Built<NullableClass, NullableClassBuilder> {
    @BuiltValueField(wireName: r'integer_prop')
    int? get integerProp;

    @BuiltValueField(wireName: r'number_prop')
    num? get numberProp;

    @BuiltValueField(wireName: r'boolean_prop')
    bool? get booleanProp;

    @BuiltValueField(wireName: r'string_prop')
    String? get stringProp;

    @BuiltValueField(wireName: r'date_prop')
    Date? get dateProp;

    @BuiltValueField(wireName: r'datetime_prop')
    DateTime? get datetimeProp;

    @BuiltValueField(wireName: r'array_nullable_prop')
    BuiltList<JsonObject>? get arrayNullableProp;

    @BuiltValueField(wireName: r'array_and_items_nullable_prop')
    BuiltList<JsonObject?>? get arrayAndItemsNullableProp;

    @BuiltValueField(wireName: r'array_items_nullable')
    BuiltList<JsonObject?>? get arrayItemsNullable;

    @BuiltValueField(wireName: r'object_nullable_prop')
    BuiltMap<String, JsonObject>? get objectNullableProp;

    @BuiltValueField(wireName: r'object_and_items_nullable_prop')
    BuiltMap<String, JsonObject?>? get objectAndItemsNullableProp;

    @BuiltValueField(wireName: r'object_items_nullable')
    BuiltMap<String, JsonObject?>? get objectItemsNullable;

    NullableClass._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(NullableClassBuilder b) => b;

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
    Iterable<Object?> serialize(Serializers serializers, NullableClass object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.integerProp != null) {
            result
                ..add(r'integer_prop')
                ..add(serializers.serialize(object.integerProp,
                    specifiedType: const FullType.nullable(int)));
        }
        if (object.numberProp != null) {
            result
                ..add(r'number_prop')
                ..add(serializers.serialize(object.numberProp,
                    specifiedType: const FullType.nullable(num)));
        }
        if (object.booleanProp != null) {
            result
                ..add(r'boolean_prop')
                ..add(serializers.serialize(object.booleanProp,
                    specifiedType: const FullType.nullable(bool)));
        }
        if (object.stringProp != null) {
            result
                ..add(r'string_prop')
                ..add(serializers.serialize(object.stringProp,
                    specifiedType: const FullType.nullable(String)));
        }
        if (object.dateProp != null) {
            result
                ..add(r'date_prop')
                ..add(serializers.serialize(object.dateProp,
                    specifiedType: const FullType.nullable(Date)));
        }
        if (object.datetimeProp != null) {
            result
                ..add(r'datetime_prop')
                ..add(serializers.serialize(object.datetimeProp,
                    specifiedType: const FullType.nullable(DateTime)));
        }
        if (object.arrayNullableProp != null) {
            result
                ..add(r'array_nullable_prop')
                ..add(serializers.serialize(object.arrayNullableProp,
                    specifiedType: const FullType.nullable(BuiltList, [FullType(JsonObject)])));
        }
        if (object.arrayAndItemsNullableProp != null) {
            result
                ..add(r'array_and_items_nullable_prop')
                ..add(serializers.serialize(object.arrayAndItemsNullableProp,
                    specifiedType: const FullType.nullable(BuiltList, [FullType.nullable(JsonObject)])));
        }
        if (object.arrayItemsNullable != null) {
            result
                ..add(r'array_items_nullable')
                ..add(serializers.serialize(object.arrayItemsNullable,
                    specifiedType: const FullType(BuiltList, [FullType.nullable(JsonObject)])));
        }
        if (object.objectNullableProp != null) {
            result
                ..add(r'object_nullable_prop')
                ..add(serializers.serialize(object.objectNullableProp,
                    specifiedType: const FullType.nullable(BuiltMap, [FullType(String), FullType(JsonObject)])));
        }
        if (object.objectAndItemsNullableProp != null) {
            result
                ..add(r'object_and_items_nullable_prop')
                ..add(serializers.serialize(object.objectAndItemsNullableProp,
                    specifiedType: const FullType.nullable(BuiltMap, [FullType(String), FullType.nullable(JsonObject)])));
        }
        if (object.objectItemsNullable != null) {
            result
                ..add(r'object_items_nullable')
                ..add(serializers.serialize(object.objectItemsNullable,
                    specifiedType: const FullType(BuiltMap, [FullType(String), FullType.nullable(JsonObject)])));
        }
        return result;
    }

    @override
    NullableClass deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = NullableClassBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'integer_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(int)) as int?;
                    if (valueDes == null) continue;
                    result.integerProp = valueDes;
                    break;
                case r'number_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(num)) as num?;
                    if (valueDes == null) continue;
                    result.numberProp = valueDes;
                    break;
                case r'boolean_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(bool)) as bool?;
                    if (valueDes == null) continue;
                    result.booleanProp = valueDes;
                    break;
                case r'string_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(String)) as String?;
                    if (valueDes == null) continue;
                    result.stringProp = valueDes;
                    break;
                case r'date_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(Date)) as Date?;
                    if (valueDes == null) continue;
                    result.dateProp = valueDes;
                    break;
                case r'datetime_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(DateTime)) as DateTime?;
                    if (valueDes == null) continue;
                    result.datetimeProp = valueDes;
                    break;
                case r'array_nullable_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(BuiltList, [FullType(JsonObject)])) as BuiltList<JsonObject>?;
                    if (valueDes == null) continue;
                    result.arrayNullableProp.replace(valueDes);
                    break;
                case r'array_and_items_nullable_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(BuiltList, [FullType.nullable(JsonObject)])) as BuiltList<JsonObject?>?;
                    if (valueDes == null) continue;
                    result.arrayAndItemsNullableProp.replace(valueDes);
                    break;
                case r'array_items_nullable':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType.nullable(JsonObject)])) as BuiltList<JsonObject?>;
                    result.arrayItemsNullable.replace(valueDes);
                    break;
                case r'object_nullable_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(BuiltMap, [FullType(String), FullType(JsonObject)])) as BuiltMap<String, JsonObject>?;
                    if (valueDes == null) continue;
                    result.objectNullableProp.replace(valueDes);
                    break;
                case r'object_and_items_nullable_prop':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType.nullable(BuiltMap, [FullType(String), FullType.nullable(JsonObject)])) as BuiltMap<String, JsonObject?>?;
                    if (valueDes == null) continue;
                    result.objectAndItemsNullableProp.replace(valueDes);
                    break;
                case r'object_items_nullable':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltMap, [FullType(String), FullType.nullable(JsonObject)])) as BuiltMap<String, JsonObject?>;
                    result.objectItemsNullable.replace(valueDes);
                    break;
            }
        }
        return result.build();
    }
}

