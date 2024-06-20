// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'enum_test.reflection.dart';
part 'enum_test.serialization.dart';


//class defination

///
mixin EnumTestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<EnumStringEnum> get enumString;
  EnumStringRequiredEnum get enumStringRequired;
  UndefinedWrapper<EnumIntegerEnum> get enumInteger;
  UndefinedWrapper<EnumIntegerOnlyEnum> get enumIntegerOnly;
  UndefinedWrapper<EnumNumberEnum> get enumNumber;
  UndefinedWrapper<OuterEnum?> get outerEnum;
  UndefinedWrapper<OuterEnumInteger> get outerEnumInteger;
  UndefinedWrapper<OuterEnumDefaultValue> get outerEnumDefaultValue;
  UndefinedWrapper<OuterEnumIntegerDefaultValue> get outerEnumIntegerDefaultValue;


}

///
class EnumTest with
$OpenApiObjectMixin,


EnumTestMixin {
  @override
  UndefinedWrapper<EnumStringEnum> enumString;
  @override
  EnumStringRequiredEnum enumStringRequired;
  @override
  UndefinedWrapper<EnumIntegerEnum> enumInteger;
  @override
  UndefinedWrapper<EnumIntegerOnlyEnum> enumIntegerOnly;
  @override
  UndefinedWrapper<EnumNumberEnum> enumNumber;
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




//inline enum def

extension type const EnumStringEnum._(String value) {
      const EnumStringEnum.UPPER() : this._(r'UPPER');
      const EnumStringEnum.lower() : this._(r'lower');
      const EnumStringEnum.empty() : this._(r'');

  /// Creates a [EnumStringEnum] enum from a value and safely checking if it exists.
  factory EnumStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumStringEnum] enum from a value without checking if it exists.
  const EnumStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumStringEnum> values = [
    EnumStringEnum.UPPER(),
    EnumStringEnum.lower(),
    EnumStringEnum.empty(),
    
  ];
}

//inline enum def

extension type const EnumStringRequiredEnum._(String value) {
      const EnumStringRequiredEnum.UPPER() : this._(r'UPPER');
      const EnumStringRequiredEnum.lower() : this._(r'lower');
      const EnumStringRequiredEnum.empty() : this._(r'');

  /// Creates a [EnumStringRequiredEnum] enum from a value and safely checking if it exists.
  factory EnumStringRequiredEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumStringRequiredEnum] enum from a value without checking if it exists.
  const EnumStringRequiredEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumStringRequiredEnum> values = [
    EnumStringRequiredEnum.UPPER(),
    EnumStringRequiredEnum.lower(),
    EnumStringRequiredEnum.empty(),
    
  ];
}

//inline enum def

extension type const EnumIntegerEnum._(int value) {
      const EnumIntegerEnum.number1() : this._(1);
      const EnumIntegerEnum.numberNegative1() : this._(-1);

  /// Creates a [EnumIntegerEnum] enum from a value and safely checking if it exists.
  factory EnumIntegerEnum.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumIntegerEnum] enum from a value without checking if it exists.
  const EnumIntegerEnum.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumIntegerEnum> values = [
    EnumIntegerEnum.number1(),
    EnumIntegerEnum.numberNegative1(),
    
  ];
}

//inline enum def

extension type const EnumIntegerOnlyEnum._(int value) {
      const EnumIntegerOnlyEnum.number2() : this._(2);
      const EnumIntegerOnlyEnum.numberNegative2() : this._(-2);

  /// Creates a [EnumIntegerOnlyEnum] enum from a value and safely checking if it exists.
  factory EnumIntegerOnlyEnum.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumIntegerOnlyEnum] enum from a value without checking if it exists.
  const EnumIntegerOnlyEnum.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumIntegerOnlyEnum> values = [
    EnumIntegerOnlyEnum.number2(),
    EnumIntegerOnlyEnum.numberNegative2(),
    
  ];
}

//inline enum def

extension type const EnumNumberEnum._(double value) {
      const EnumNumberEnum.number11() : this._(1.1);
      const EnumNumberEnum.numberNegative12() : this._(-1.2);

  /// Creates a [EnumNumberEnum] enum from a value and safely checking if it exists.
  factory EnumNumberEnum.$safe(double value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumNumberEnum] enum from a value without checking if it exists.
  const EnumNumberEnum.$unsafe(double value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumNumberEnum> values = [
    EnumNumberEnum.number11(),
    EnumNumberEnum.numberNegative12(),
    
  ];
}

