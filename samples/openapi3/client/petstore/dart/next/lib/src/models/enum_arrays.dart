// Model def

import 'package:openapi/_internal.dart';


part 'enum_arrays.reflection.dart';
part 'enum_arrays.serialization.dart';


/// EnumArraysMixin
///
/// Properties:
/// * [justSymbol] 
/// * [arrayEnum] 
mixin EnumArraysMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            EnumArraysJustSymbolEnum
> get justSymbol;
UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
> get arrayEnum;
  
}

/// EnumArrays
///
/// Properties:
/// * [justSymbol] 
/// * [arrayEnum] 
class EnumArrays with
$OpenApiObjectMixin,


EnumArraysMixin {
  @override
  UndefinedWrapper<
            EnumArraysJustSymbolEnum
> justSymbol;
  @override
  UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
> arrayEnum;

  

  

  EnumArrays.$all({
        required this.justSymbol,
    required this.arrayEnum,
    
    
  });

  EnumArrays({
      this.justSymbol = const UndefinedWrapper
        .undefined()
,
  this.arrayEnum = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = EnumArraysReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$EnumArraysToMap(this);
  }
  factory EnumArrays.fromMap(Map<String, dynamic> src) {
    return _$EnumArraysFromMap(src);
  }
  static EnumArrays? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return EnumArrays.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$EnumArraysCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory EnumArrays.deserialize(Object? src) {
    return _$EnumArraysDeserialize(src);
  }
  static EnumArrays? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return EnumArrays.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$EnumArraysCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$EnumArraysSerialize(this);
  }
}




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

