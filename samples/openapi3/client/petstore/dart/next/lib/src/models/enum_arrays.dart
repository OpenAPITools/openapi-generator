// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'enum_arrays.reflection.dart';
part 'enum_arrays.serialization.dart';

//class defination

///
mixin EnumArraysMixin on $OpenApiObjectMixin {
  UndefinedWrapper<EnumArraysJustSymbolEnum> get justSymbol;
  UndefinedWrapper<List<EnumArraysArrayEnumEnum>> get arrayEnum;
}

///
class EnumArrays with $OpenApiObjectMixin, EnumArraysMixin {
  @override
  UndefinedWrapper<EnumArraysJustSymbolEnum> justSymbol;
  @override
  UndefinedWrapper<List<EnumArraysArrayEnumEnum>> arrayEnum;

  EnumArrays.$all({
    required this.justSymbol,
    required this.arrayEnum,
  });

  EnumArrays({
    this.justSymbol = const UndefinedWrapper.undefined(),
    this.arrayEnum = const UndefinedWrapper.undefined(),
  });
}

//inline enum def

extension type const EnumArraysJustSymbolEnum._(String value) {
  const EnumArraysJustSymbolEnum.greaterThanEqual() : this._(r'>=');
  const EnumArraysJustSymbolEnum.value() : this._(r'$');

  /// Creates a [EnumArraysJustSymbolEnum] enum from a value and safely checking if it exists.
  factory EnumArraysJustSymbolEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumArraysJustSymbolEnum] enum from a value without checking if it exists.
  const EnumArraysJustSymbolEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumArraysJustSymbolEnum> values = [
    EnumArraysJustSymbolEnum.greaterThanEqual(),
    EnumArraysJustSymbolEnum.value(),
  ];
}

//inline enum def

extension type const EnumArraysArrayEnumEnum._(String value) {
  const EnumArraysArrayEnumEnum.fish() : this._(r'fish');
  const EnumArraysArrayEnumEnum.crab() : this._(r'crab');

  /// Creates a [EnumArraysArrayEnumEnum] enum from a value and safely checking if it exists.
  factory EnumArraysArrayEnumEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumArraysArrayEnumEnum] enum from a value without checking if it exists.
  const EnumArraysArrayEnumEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumArraysArrayEnumEnum> values = [
    EnumArraysArrayEnumEnum.fish(),
    EnumArraysArrayEnumEnum.crab(),
  ];
}
