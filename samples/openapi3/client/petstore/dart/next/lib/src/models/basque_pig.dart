// Model def

import 'package:openapi/_internal.dart';


part 'basque_pig.reflection.dart';
part 'basque_pig.serialization.dart';


/// BasquePigMixin
///
/// Properties:
/// * [className] 
mixin BasquePigMixin on
  $OpenApiObjectMixin {
  
            String
 get className;
  
}

/// BasquePig
///
/// Properties:
/// * [className] 
class BasquePig with
$OpenApiObjectMixin,


BasquePigMixin {
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  

  BasquePig.$all({
        required this.className,
    required this.additionalProperties,
    
  });

  BasquePig({
    required  this.className     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = BasquePigReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$BasquePigToMap(this);
  }
  factory BasquePig.fromMap(Map<String, dynamic> src) {
    return _$BasquePigFromMap(src);
  }
  static BasquePig? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return BasquePig.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$BasquePigCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory BasquePig.deserialize(Object? src) {
    return _$BasquePigDeserialize(src);
  }
  static BasquePig? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return BasquePig.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$BasquePigCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$BasquePigSerialize(this);
  }
}




