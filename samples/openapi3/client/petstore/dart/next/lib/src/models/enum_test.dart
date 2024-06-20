// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'enum_test.reflection.dart';
part 'enum_test.serialization.dart';


/// EnumTestMixin
///
/// Properties:
/// * [enumString] 
/// * [enumStringRequired] 
/// * [enumInteger] 
/// * [enumIntegerOnly] 
/// * [enumNumber] 
/// * [outerEnum] 
/// * [outerEnumInteger] 
/// * [outerEnumDefaultValue] 
/// * [outerEnumIntegerDefaultValue] 
mixin EnumTestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<EnumTestEnumStringEnum> get enumString;
  EnumTestEnumStringRequiredEnum get enumStringRequired;
  UndefinedWrapper<EnumTestEnumIntegerEnum> get enumInteger;
  UndefinedWrapper<EnumTestEnumIntegerOnlyEnum> get enumIntegerOnly;
  UndefinedWrapper<EnumTestEnumNumberEnum> get enumNumber;
  UndefinedWrapper<OuterEnum?> get outerEnum;
  UndefinedWrapper<OuterEnumInteger> get outerEnumInteger;
  UndefinedWrapper<OuterEnumDefaultValue> get outerEnumDefaultValue;
  UndefinedWrapper<OuterEnumIntegerDefaultValue> get outerEnumIntegerDefaultValue;

}

/// EnumTest
///
/// Properties:
/// * [enumString] 
/// * [enumStringRequired] 
/// * [enumInteger] 
/// * [enumIntegerOnly] 
/// * [enumNumber] 
/// * [outerEnum] 
/// * [outerEnumInteger] 
/// * [outerEnumDefaultValue] 
/// * [outerEnumIntegerDefaultValue] 
class EnumTest with
$OpenApiObjectMixin,


EnumTestMixin {
  @override
  UndefinedWrapper<EnumTestEnumStringEnum> enumString;
  @override
  EnumTestEnumStringRequiredEnum enumStringRequired;
  @override
  UndefinedWrapper<EnumTestEnumIntegerEnum> enumInteger;
  @override
  UndefinedWrapper<EnumTestEnumIntegerOnlyEnum> enumIntegerOnly;
  @override
  UndefinedWrapper<EnumTestEnumNumberEnum> enumNumber;
  @override
  UndefinedWrapper<OuterEnum?> outerEnum;
  @override
  UndefinedWrapper<OuterEnumInteger> outerEnumInteger;
  @override
  UndefinedWrapper<OuterEnumDefaultValue> outerEnumDefaultValue;
  @override
  UndefinedWrapper<OuterEnumIntegerDefaultValue> outerEnumIntegerDefaultValue;





  EnumTest.$all({
    required this.enumString,
    required this.enumStringRequired,
    required this.enumInteger,
    required this.enumIntegerOnly,
    required this.enumNumber,
    required this.outerEnum,
    required this.outerEnumInteger,
    required this.outerEnumDefaultValue,
    required this.outerEnumIntegerDefaultValue,
    
    
  });

  EnumTest({
    this.enumString = const UndefinedWrapper.undefined(),
  required  this.enumStringRequired ,
    this.enumInteger = const UndefinedWrapper.undefined(),
    this.enumIntegerOnly = const UndefinedWrapper.undefined(),
    this.enumNumber = const UndefinedWrapper.undefined(),
    this.outerEnum = const UndefinedWrapper.undefined(),
    this.outerEnumInteger = const UndefinedWrapper.undefined(),
    this.outerEnumDefaultValue = const UndefinedWrapper.undefined(),
    this.outerEnumIntegerDefaultValue = const UndefinedWrapper.undefined(),
    
    
  });
}




extension type const EnumTestEnumStringEnum._(String value) {
      const EnumTestEnumStringEnum.UPPER() : this._(r'UPPER');
      const EnumTestEnumStringEnum.lower() : this._(r'lower');
      const EnumTestEnumStringEnum.empty() : this._(r'');

  /// Creates a [EnumTestEnumStringEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumTestEnumStringEnum] enum from a value without checking if it exists.
  const EnumTestEnumStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumStringEnum> values = [
    EnumTestEnumStringEnum.UPPER(),
    EnumTestEnumStringEnum.lower(),
    EnumTestEnumStringEnum.empty(),
    
  ];
}

extension type const EnumTestEnumStringRequiredEnum._(String value) {
      const EnumTestEnumStringRequiredEnum.UPPER() : this._(r'UPPER');
      const EnumTestEnumStringRequiredEnum.lower() : this._(r'lower');
      const EnumTestEnumStringRequiredEnum.empty() : this._(r'');

  /// Creates a [EnumTestEnumStringRequiredEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumStringRequiredEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumTestEnumStringRequiredEnum] enum from a value without checking if it exists.
  const EnumTestEnumStringRequiredEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumStringRequiredEnum> values = [
    EnumTestEnumStringRequiredEnum.UPPER(),
    EnumTestEnumStringRequiredEnum.lower(),
    EnumTestEnumStringRequiredEnum.empty(),
    
  ];
}

extension type const EnumTestEnumIntegerEnum._(int value) {
      const EnumTestEnumIntegerEnum.number1() : this._(1);
      const EnumTestEnumIntegerEnum.numberNegative1() : this._(-1);

  /// Creates a [EnumTestEnumIntegerEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumIntegerEnum.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumTestEnumIntegerEnum] enum from a value without checking if it exists.
  const EnumTestEnumIntegerEnum.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumIntegerEnum> values = [
    EnumTestEnumIntegerEnum.number1(),
    EnumTestEnumIntegerEnum.numberNegative1(),
    
  ];
}

extension type const EnumTestEnumIntegerOnlyEnum._(int value) {
      const EnumTestEnumIntegerOnlyEnum.number2() : this._(2);
      const EnumTestEnumIntegerOnlyEnum.numberNegative2() : this._(-2);

  /// Creates a [EnumTestEnumIntegerOnlyEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumIntegerOnlyEnum.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumTestEnumIntegerOnlyEnum] enum from a value without checking if it exists.
  const EnumTestEnumIntegerOnlyEnum.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumIntegerOnlyEnum> values = [
    EnumTestEnumIntegerOnlyEnum.number2(),
    EnumTestEnumIntegerOnlyEnum.numberNegative2(),
    
  ];
}

extension type const EnumTestEnumNumberEnum._(double value) {
      const EnumTestEnumNumberEnum.number11() : this._(1.1);
      const EnumTestEnumNumberEnum.numberNegative12() : this._(-1.2);

  /// Creates a [EnumTestEnumNumberEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumNumberEnum.$safe(double value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumTestEnumNumberEnum] enum from a value without checking if it exists.
  const EnumTestEnumNumberEnum.$unsafe(double value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumNumberEnum> values = [
    EnumTestEnumNumberEnum.number11(),
    EnumTestEnumNumberEnum.numberNegative12(),
    
  ];
}

