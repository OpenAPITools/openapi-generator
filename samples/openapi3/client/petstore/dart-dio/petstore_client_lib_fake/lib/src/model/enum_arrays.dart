//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'enum_arrays.g.dart';

/// EnumArrays
///
/// Properties:
/// * [justSymbol] 
/// * [arrayEnum] 
@BuiltValue()
abstract class EnumArrays implements Built<EnumArrays, EnumArraysBuilder> {
  @BuiltValueField(wireName: r'just_symbol')
  EnumArraysJustSymbolEnum? get justSymbol;
  // enum justSymbolEnum {  >=,  $,  };

  @BuiltValueField(wireName: r'array_enum')
  BuiltList<EnumArraysArrayEnumEnum>? get arrayEnum;
  // enum arrayEnumEnum {  fish,  crab,  };

  EnumArrays._();

  factory EnumArrays([void updates(EnumArraysBuilder b)]) = _$EnumArrays;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(EnumArraysBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<EnumArrays> get serializer => _$EnumArraysSerializer();
}

class _$EnumArraysSerializer implements PrimitiveSerializer<EnumArrays> {
  @override
  final Iterable<Type> types = const [EnumArrays, _$EnumArrays];

  @override
  final String wireName = r'EnumArrays';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    EnumArrays object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.justSymbol != null) {
      yield r'just_symbol';
      yield serializers.serialize(
        object.justSymbol,
        specifiedType: const FullType(EnumArraysJustSymbolEnum),
      );
    }
    if (object.arrayEnum != null) {
      yield r'array_enum';
      yield serializers.serialize(
        object.arrayEnum,
        specifiedType: const FullType(BuiltList, [FullType(EnumArraysArrayEnumEnum)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    EnumArrays object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required EnumArraysBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'just_symbol':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(EnumArraysJustSymbolEnum),
          ) as EnumArraysJustSymbolEnum;
          result.justSymbol = valueDes;
          break;
        case r'array_enum':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(EnumArraysArrayEnumEnum)]),
          ) as BuiltList<EnumArraysArrayEnumEnum>;
          result.arrayEnum.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  EnumArrays deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = EnumArraysBuilder();
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

class EnumArraysJustSymbolEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'>=')
  static const EnumArraysJustSymbolEnum greaterThanEqual = _$enumArraysJustSymbolEnum_greaterThanEqual;
  @BuiltValueEnumConst(wireName: r'$')
  static const EnumArraysJustSymbolEnum dollar = _$enumArraysJustSymbolEnum_dollar;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const EnumArraysJustSymbolEnum unknownDefaultOpenApi = _$enumArraysJustSymbolEnum_unknownDefaultOpenApi;

  static Serializer<EnumArraysJustSymbolEnum> get serializer => _$enumArraysJustSymbolEnumSerializer;

  const EnumArraysJustSymbolEnum._(String name): super(name);

  static BuiltSet<EnumArraysJustSymbolEnum> get values => _$enumArraysJustSymbolEnumValues;
  static EnumArraysJustSymbolEnum valueOf(String name) => _$enumArraysJustSymbolEnumValueOf(name);
}

class EnumArraysArrayEnumEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'fish')
  static const EnumArraysArrayEnumEnum fish = _$enumArraysArrayEnumEnum_fish;
  @BuiltValueEnumConst(wireName: r'crab')
  static const EnumArraysArrayEnumEnum crab = _$enumArraysArrayEnumEnum_crab;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const EnumArraysArrayEnumEnum unknownDefaultOpenApi = _$enumArraysArrayEnumEnum_unknownDefaultOpenApi;

  static Serializer<EnumArraysArrayEnumEnum> get serializer => _$enumArraysArrayEnumEnumSerializer;

  const EnumArraysArrayEnumEnum._(String name): super(name);

  static BuiltSet<EnumArraysArrayEnumEnum> get values => _$enumArraysArrayEnumEnumValues;
  static EnumArraysArrayEnumEnum valueOf(String name) => _$enumArraysArrayEnumEnumValueOf(name);
}

