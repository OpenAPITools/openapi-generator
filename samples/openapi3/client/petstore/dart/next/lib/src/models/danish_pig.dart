// Model def

import 'package:openapi/_internal.dart';


part 'danish_pig.reflection.dart';
part 'danish_pig.serialization.dart';


/// DanishPigMixin
///
/// Properties:
/// * [className] 
mixin DanishPigMixin on
  $OpenApiObjectMixin {
  
            String
 get className;
  
}

/// DanishPig
///
/// Properties:
/// * [className] 
class DanishPig with
$OpenApiObjectMixin,


DanishPigMixin {
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  

  DanishPig.$all({
        required this.className,
    required this.additionalProperties,
    
  });

  DanishPig({
    required  this.className     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = DanishPigReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$DanishPigToMap(this);
  }
  factory DanishPig.fromMap(Map<String, dynamic> src) {
    return _$DanishPigFromMap(src);
  }
  static DanishPig? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return DanishPig.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$DanishPigCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory DanishPig.deserialize(Object? src) {
    return _$DanishPigDeserialize(src);
  }
  static DanishPig? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return DanishPig.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$DanishPigCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$DanishPigSerialize(this);
  }
}




