// Model def

import 'package:openapi/_internal.dart';


part 'enum_string_discriminator.reflection.dart';
part 'enum_string_discriminator.serialization.dart';


/// An object to test discriminator of enum string
///
/// Properties:
/// * [enumStrType] - enum string type
mixin EnumStringDiscriminatorMixin on 
  $OpenApiObjectMixin {
  
            EnumStringDiscriminatorEnumStrTypeEnum
 get enumStrType;
  
}

/// An object to test discriminator of enum string
///
/// Properties:
/// * [enumStrType] - enum string type
class EnumStringDiscriminator with
$OpenApiObjectMixin,


EnumStringDiscriminatorMixin {
  @override
  
            EnumStringDiscriminatorEnumStrTypeEnum
 enumStrType;

  

  

  EnumStringDiscriminator.$all({
        required this.enumStrType,
    
    
  });

  EnumStringDiscriminator({
    required  this.enumStrType     ,
    
    
  });

  static const $reflection = EnumStringDiscriminatorReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$EnumStringDiscriminatorToMap(this);
  }
  factory EnumStringDiscriminator.fromMap(Map<String, dynamic> src) {
    return _$EnumStringDiscriminatorFromMap(src);
  }
  static EnumStringDiscriminator? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return EnumStringDiscriminator.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$EnumStringDiscriminatorCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory EnumStringDiscriminator.deserialize(Object? src) {
    return _$EnumStringDiscriminatorDeserialize(src);
  }
  static EnumStringDiscriminator? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return EnumStringDiscriminator.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$EnumStringDiscriminatorCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$EnumStringDiscriminatorSerialize(this);
  }
}




extension type const EnumStringDiscriminatorEnumStrTypeEnum._(String value) {
  /// enum string type
      const EnumStringDiscriminatorEnumStrTypeEnum.a() : this._(r'type_a');
  /// enum string type
      const EnumStringDiscriminatorEnumStrTypeEnum.b() : this._(r'type_b');

  /// Creates a [EnumStringDiscriminatorEnumStrTypeEnum] enum from a value and safely checking if it exists.
  factory EnumStringDiscriminatorEnumStrTypeEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumStringDiscriminatorEnumStrTypeEnum] enum from a value without checking if it exists.
  const EnumStringDiscriminatorEnumStrTypeEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumStringDiscriminatorEnumStrTypeEnum> values = [
    EnumStringDiscriminatorEnumStrTypeEnum.a(),
    EnumStringDiscriminatorEnumStrTypeEnum.b(),
    
  ];
}

