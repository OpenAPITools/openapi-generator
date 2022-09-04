//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/outer_enum.dart';
import 'package:openapi/src/model/outer_enum_default_value.dart';
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/outer_enum_integer.dart';
import 'package:openapi/src/model/outer_enum_integer_default_value.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'enum_test.g.dart';

/// EnumTest
///
/// Properties:
/// * [enumString] 
/// * [enumStringRequired] 
/// * [enumInteger] 
/// * [enumNumber] 
/// * [outerEnum] 
/// * [outerEnumInteger] 
/// * [outerEnumDefaultValue] 
/// * [outerEnumIntegerDefaultValue] 
@BuiltValue()
abstract class EnumTest implements Built<EnumTest, EnumTestBuilder> {
  @BuiltValueField(wireName: r'enum_string')
  EnumTestEnumStringEnum? get enumString;
  // enum enumStringEnum {  UPPER,  lower,  ,  };

  @BuiltValueField(wireName: r'enum_string_required')
  EnumTestEnumStringRequiredEnum get enumStringRequired;
  // enum enumStringRequiredEnum {  UPPER,  lower,  ,  };

  @BuiltValueField(wireName: r'enum_integer')
  EnumTestEnumIntegerEnum? get enumInteger;
  // enum enumIntegerEnum {  1,  -1,  };

  @BuiltValueField(wireName: r'enum_number')
  EnumTestEnumNumberEnum? get enumNumber;
  // enum enumNumberEnum {  1.1,  -1.2,  };

  @BuiltValueField(wireName: r'outerEnum')
  OuterEnum? get outerEnum;
  // enum outerEnumEnum {  placed,  approved,  delivered,  };

  @BuiltValueField(wireName: r'outerEnumInteger')
  OuterEnumInteger? get outerEnumInteger;
  // enum outerEnumIntegerEnum {  0,  1,  2,  };

  @BuiltValueField(wireName: r'outerEnumDefaultValue')
  OuterEnumDefaultValue? get outerEnumDefaultValue;
  // enum outerEnumDefaultValueEnum {  placed,  approved,  delivered,  };

  @BuiltValueField(wireName: r'outerEnumIntegerDefaultValue')
  OuterEnumIntegerDefaultValue? get outerEnumIntegerDefaultValue;
  // enum outerEnumIntegerDefaultValueEnum {  0,  1,  2,  };

  EnumTest._();

  factory EnumTest([void updates(EnumTestBuilder b)]) = _$EnumTest;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(EnumTestBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<EnumTest> get serializer => _$EnumTestSerializer();
}

class _$EnumTestSerializer implements PrimitiveSerializer<EnumTest> {
  @override
  final Iterable<Type> types = const [EnumTest, _$EnumTest];

  @override
  final String wireName = r'EnumTest';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    EnumTest object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.enumString != null) {
      yield r'enum_string';
      yield serializers.serialize(
        object.enumString,
        specifiedType: const FullType(EnumTestEnumStringEnum),
      );
    }
    yield r'enum_string_required';
    yield serializers.serialize(
      object.enumStringRequired,
      specifiedType: const FullType(EnumTestEnumStringRequiredEnum),
    );
    if (object.enumInteger != null) {
      yield r'enum_integer';
      yield serializers.serialize(
        object.enumInteger,
        specifiedType: const FullType(EnumTestEnumIntegerEnum),
      );
    }
    if (object.enumNumber != null) {
      yield r'enum_number';
      yield serializers.serialize(
        object.enumNumber,
        specifiedType: const FullType(EnumTestEnumNumberEnum),
      );
    }
    if (object.outerEnum != null) {
      yield r'outerEnum';
      yield serializers.serialize(
        object.outerEnum,
        specifiedType: const FullType.nullable(OuterEnum),
      );
    }
    if (object.outerEnumInteger != null) {
      yield r'outerEnumInteger';
      yield serializers.serialize(
        object.outerEnumInteger,
        specifiedType: const FullType(OuterEnumInteger),
      );
    }
    if (object.outerEnumDefaultValue != null) {
      yield r'outerEnumDefaultValue';
      yield serializers.serialize(
        object.outerEnumDefaultValue,
        specifiedType: const FullType(OuterEnumDefaultValue),
      );
    }
    if (object.outerEnumIntegerDefaultValue != null) {
      yield r'outerEnumIntegerDefaultValue';
      yield serializers.serialize(
        object.outerEnumIntegerDefaultValue,
        specifiedType: const FullType(OuterEnumIntegerDefaultValue),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    EnumTest object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required EnumTestBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'enum_string':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(EnumTestEnumStringEnum),
          ) as EnumTestEnumStringEnum;
          result.enumString = valueDes;
          break;
        case r'enum_string_required':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(EnumTestEnumStringRequiredEnum),
          ) as EnumTestEnumStringRequiredEnum;
          result.enumStringRequired = valueDes;
          break;
        case r'enum_integer':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(EnumTestEnumIntegerEnum),
          ) as EnumTestEnumIntegerEnum;
          result.enumInteger = valueDes;
          break;
        case r'enum_number':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(EnumTestEnumNumberEnum),
          ) as EnumTestEnumNumberEnum;
          result.enumNumber = valueDes;
          break;
        case r'outerEnum':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType.nullable(OuterEnum),
          ) as OuterEnum?;
          if (valueDes == null) continue;
          result.outerEnum = valueDes;
          break;
        case r'outerEnumInteger':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(OuterEnumInteger),
          ) as OuterEnumInteger;
          result.outerEnumInteger = valueDes;
          break;
        case r'outerEnumDefaultValue':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(OuterEnumDefaultValue),
          ) as OuterEnumDefaultValue;
          result.outerEnumDefaultValue = valueDes;
          break;
        case r'outerEnumIntegerDefaultValue':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(OuterEnumIntegerDefaultValue),
          ) as OuterEnumIntegerDefaultValue;
          result.outerEnumIntegerDefaultValue = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  EnumTest deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = EnumTestBuilder();
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

class EnumTestEnumStringEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const EnumTestEnumStringEnum UPPER = _$enumTestEnumStringEnum_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const EnumTestEnumStringEnum lower = _$enumTestEnumStringEnum_lower;
  @BuiltValueEnumConst(wireName: r'')
  static const EnumTestEnumStringEnum empty = _$enumTestEnumStringEnum_empty;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const EnumTestEnumStringEnum unknownDefaultOpenApi = _$enumTestEnumStringEnum_unknownDefaultOpenApi;

  static Serializer<EnumTestEnumStringEnum> get serializer => _$enumTestEnumStringEnumSerializer;

  const EnumTestEnumStringEnum._(String name): super(name);

  static BuiltSet<EnumTestEnumStringEnum> get values => _$enumTestEnumStringEnumValues;
  static EnumTestEnumStringEnum valueOf(String name) => _$enumTestEnumStringEnumValueOf(name);
}

class EnumTestEnumStringRequiredEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const EnumTestEnumStringRequiredEnum UPPER = _$enumTestEnumStringRequiredEnum_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const EnumTestEnumStringRequiredEnum lower = _$enumTestEnumStringRequiredEnum_lower;
  @BuiltValueEnumConst(wireName: r'')
  static const EnumTestEnumStringRequiredEnum empty = _$enumTestEnumStringRequiredEnum_empty;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const EnumTestEnumStringRequiredEnum unknownDefaultOpenApi = _$enumTestEnumStringRequiredEnum_unknownDefaultOpenApi;

  static Serializer<EnumTestEnumStringRequiredEnum> get serializer => _$enumTestEnumStringRequiredEnumSerializer;

  const EnumTestEnumStringRequiredEnum._(String name): super(name);

  static BuiltSet<EnumTestEnumStringRequiredEnum> get values => _$enumTestEnumStringRequiredEnumValues;
  static EnumTestEnumStringRequiredEnum valueOf(String name) => _$enumTestEnumStringRequiredEnumValueOf(name);
}

class EnumTestEnumIntegerEnum extends EnumClass {

  @BuiltValueEnumConst(wireNumber: 1)
  static const EnumTestEnumIntegerEnum number1 = _$enumTestEnumIntegerEnum_number1;
  @BuiltValueEnumConst(wireNumber: -1)
  static const EnumTestEnumIntegerEnum numberNegative1 = _$enumTestEnumIntegerEnum_numberNegative1;
  @BuiltValueEnumConst(wireNumber: 11184809, fallback: true)
  static const EnumTestEnumIntegerEnum unknownDefaultOpenApi = _$enumTestEnumIntegerEnum_unknownDefaultOpenApi;

  static Serializer<EnumTestEnumIntegerEnum> get serializer => _$enumTestEnumIntegerEnumSerializer;

  const EnumTestEnumIntegerEnum._(String name): super(name);

  static BuiltSet<EnumTestEnumIntegerEnum> get values => _$enumTestEnumIntegerEnumValues;
  static EnumTestEnumIntegerEnum valueOf(String name) => _$enumTestEnumIntegerEnumValueOf(name);
}

class EnumTestEnumNumberEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'1.1')
  static const EnumTestEnumNumberEnum number1Period1 = _$enumTestEnumNumberEnum_number1Period1;
  @BuiltValueEnumConst(wireName: r'-1.2')
  static const EnumTestEnumNumberEnum numberNegative1Period2 = _$enumTestEnumNumberEnum_numberNegative1Period2;
  @BuiltValueEnumConst(wireName: r'11184809', fallback: true)
  static const EnumTestEnumNumberEnum unknownDefaultOpenApi = _$enumTestEnumNumberEnum_unknownDefaultOpenApi;

  static Serializer<EnumTestEnumNumberEnum> get serializer => _$enumTestEnumNumberEnumSerializer;

  const EnumTestEnumNumberEnum._(String name): super(name);

  static BuiltSet<EnumTestEnumNumberEnum> get values => _$enumTestEnumNumberEnumValues;
  static EnumTestEnumNumberEnum valueOf(String name) => _$enumTestEnumNumberEnumValueOf(name);
}

