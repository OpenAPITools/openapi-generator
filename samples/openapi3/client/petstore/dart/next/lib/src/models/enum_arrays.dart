// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'enum_arrays.reflection.dart';
part 'enum_arrays.serialization.dart';


//class defination

///
mixin EnumArraysMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<JustSymbolEnum> get justSymbol;
  UndefinedWrapper<List<ArrayEnumEnum>> get arrayEnum;


}

///
class EnumArrays with
$OpenApiObjectMixin,


EnumArraysMixin {
  @override
  UndefinedWrapper<JustSymbolEnum> justSymbol;
  @override
  UndefinedWrapper<List<ArrayEnumEnum>> arrayEnum;





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

extension type const JustSymbolEnum._(String value) {
      const JustSymbolEnum.greaterThanEqual() : this._(r'>=');
      const JustSymbolEnum.value() : this._(r'$');

  /// Creates a [JustSymbolEnum] enum from a value and safely checking if it exists.
  factory JustSymbolEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [JustSymbolEnum] enum from a value without checking if it exists.
  const JustSymbolEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<JustSymbolEnum> values = [
    JustSymbolEnum.greaterThanEqual(),
    JustSymbolEnum.value(),
    
  ];
}

//inline enum def

extension type const ArrayEnumEnum._(String value) {
      const ArrayEnumEnum.fish() : this._(r'fish');
      const ArrayEnumEnum.crab() : this._(r'crab');

  /// Creates a [ArrayEnumEnum] enum from a value and safely checking if it exists.
  factory ArrayEnumEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [ArrayEnumEnum] enum from a value without checking if it exists.
  const ArrayEnumEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<ArrayEnumEnum> values = [
    ArrayEnumEnum.fish(),
    ArrayEnumEnum.crab(),
    
  ];
}

