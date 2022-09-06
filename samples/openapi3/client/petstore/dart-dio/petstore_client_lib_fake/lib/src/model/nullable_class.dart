//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
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
@BuiltValue()
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

  factory NullableClass([void updates(NullableClassBuilder b)]) = _$NullableClass;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(NullableClassBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<NullableClass> get serializer => _$NullableClassSerializer();
}

class _$NullableClassSerializer implements PrimitiveSerializer<NullableClass> {
  @override
  final Iterable<Type> types = const [NullableClass, _$NullableClass];

  @override
  final String wireName = r'NullableClass';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    NullableClass object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.integerProp != null) {
      yield r'integer_prop';
      yield serializers.serialize(
        object.integerProp,
        specifiedType: const FullType.nullable(int),
      );
    }
    if (object.numberProp != null) {
      yield r'number_prop';
      yield serializers.serialize(
        object.numberProp,
        specifiedType: const FullType.nullable(num),
      );
    }
    if (object.booleanProp != null) {
      yield r'boolean_prop';
      yield serializers.serialize(
        object.booleanProp,
        specifiedType: const FullType.nullable(bool),
      );
    }
    if (object.stringProp != null) {
      yield r'string_prop';
      yield serializers.serialize(
        object.stringProp,
        specifiedType: const FullType.nullable(String),
      );
    }
    if (object.dateProp != null) {
      yield r'date_prop';
      yield serializers.serialize(
        object.dateProp,
        specifiedType: const FullType.nullable(Date),
      );
    }
    if (object.datetimeProp != null) {
      yield r'datetime_prop';
      yield serializers.serialize(
        object.datetimeProp,
        specifiedType: const FullType.nullable(DateTime),
      );
    }
    if (object.arrayNullableProp != null) {
      yield r'array_nullable_prop';
      yield serializers.serialize(
        object.arrayNullableProp,
        specifiedType: const FullType.nullable(BuiltList, [FullType(JsonObject)]),
      );
    }
    if (object.arrayAndItemsNullableProp != null) {
      yield r'array_and_items_nullable_prop';
      yield serializers.serialize(
        object.arrayAndItemsNullableProp,
        specifiedType: const FullType.nullable(BuiltList, [FullType.nullable(JsonObject)]),
      );
    }
    if (object.arrayItemsNullable != null) {
      yield r'array_items_nullable';
      yield serializers.serialize(
        object.arrayItemsNullable,
        specifiedType: const FullType(BuiltList, [FullType.nullable(JsonObject)]),
      );
    }
    if (object.objectNullableProp != null) {
      yield r'object_nullable_prop';
      yield serializers.serialize(
        object.objectNullableProp,
        specifiedType: const FullType.nullable(BuiltMap, [FullType(String), FullType(JsonObject)]),
      );
    }
    if (object.objectAndItemsNullableProp != null) {
      yield r'object_and_items_nullable_prop';
      yield serializers.serialize(
        object.objectAndItemsNullableProp,
        specifiedType: const FullType.nullable(BuiltMap, [FullType(String), FullType.nullable(JsonObject)]),
      );
    }
    if (object.objectItemsNullable != null) {
      yield r'object_items_nullable';
      yield serializers.serialize(
        object.objectItemsNullable,
        specifiedType: const FullType(BuiltMap, [FullType(String), FullType.nullable(JsonObject)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    NullableClass object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required NullableClassBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'integer_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(int),
          ) as int?;
          if (valueDes == null) continue;
          result.integerProp = valueDes;
          break;
        case r'number_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(num),
          ) as num?;
          if (valueDes == null) continue;
          result.numberProp = valueDes;
          break;
        case r'boolean_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(bool),
          ) as bool?;
          if (valueDes == null) continue;
          result.booleanProp = valueDes;
          break;
        case r'string_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(String),
          ) as String?;
          if (valueDes == null) continue;
          result.stringProp = valueDes;
          break;
        case r'date_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(Date),
          ) as Date?;
          if (valueDes == null) continue;
          result.dateProp = valueDes;
          break;
        case r'datetime_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(DateTime),
          ) as DateTime?;
          if (valueDes == null) continue;
          result.datetimeProp = valueDes;
          break;
        case r'array_nullable_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(BuiltList, [FullType(JsonObject)]),
          ) as BuiltList<JsonObject>?;
          if (valueDes == null) continue;
          result.arrayNullableProp.replace(valueDes);
          break;
        case r'array_and_items_nullable_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(BuiltList, [FullType.nullable(JsonObject)]),
          ) as BuiltList<JsonObject?>?;
          if (valueDes == null) continue;
          result.arrayAndItemsNullableProp.replace(valueDes);
          break;
        case r'array_items_nullable':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType.nullable(JsonObject)]),
          ) as BuiltList<JsonObject?>;
          result.arrayItemsNullable.replace(valueDes);
          break;
        case r'object_nullable_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(BuiltMap, [FullType(String), FullType(JsonObject)]),
          ) as BuiltMap<String, JsonObject>?;
          if (valueDes == null) continue;
          result.objectNullableProp.replace(valueDes);
          break;
        case r'object_and_items_nullable_prop':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(BuiltMap, [FullType(String), FullType.nullable(JsonObject)]),
          ) as BuiltMap<String, JsonObject?>?;
          if (valueDes == null) continue;
          result.objectAndItemsNullableProp.replace(valueDes);
          break;
        case r'object_items_nullable':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltMap, [FullType(String), FullType.nullable(JsonObject)]),
          ) as BuiltMap<String, JsonObject?>;
          result.objectItemsNullable.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  NullableClass deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = NullableClassBuilder();
    final serializedList = (serialized as Iterable<Object?>).toList();
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result,
    );
    return result.build();
  }
}

