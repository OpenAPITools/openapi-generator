//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'enum_arrays.g.dart';

abstract class EnumArrays implements Built<EnumArrays, EnumArraysBuilder> {

    @nullable
    @BuiltValueField(wireName: r'just_symbol')
    EnumArraysJustSymbolEnum get justSymbol;
    // enum justSymbolEnum {  >=,  $,  };

    @nullable
    @BuiltValueField(wireName: r'array_enum')
    BuiltList<EnumArraysArrayEnumEnum> get arrayEnum;
    // enum arrayEnumEnum {  fish,  crab,  };

    // Boilerplate code needed to wire-up generated code
    EnumArrays._();

    static void _initializeBuilder(EnumArraysBuilder b) => b;

    factory EnumArrays([void updates(EnumArraysBuilder b)]) = _$EnumArrays;
    static Serializer<EnumArrays> get serializer => _$enumArraysSerializer;
}

class EnumArraysJustSymbolEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'>=')
  static const EnumArraysJustSymbolEnum greaterThanEqual = _$enumArraysJustSymbolEnum_greaterThanEqual;
  @BuiltValueEnumConst(wireName: r'\$')
  static const EnumArraysJustSymbolEnum dollar = _$enumArraysJustSymbolEnum_dollar;

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

  static Serializer<EnumArraysArrayEnumEnum> get serializer => _$enumArraysArrayEnumEnumSerializer;

  const EnumArraysArrayEnumEnum._(String name): super(name);

  static BuiltSet<EnumArraysArrayEnumEnum> get values => _$enumArraysArrayEnumEnumValues;
  static EnumArraysArrayEnumEnum valueOf(String name) => _$enumArraysArrayEnumEnumValueOf(name);
}

