// Model def

import 'package:openapi/_internal.dart';


part 'triangle_interface.reflection.dart';
part 'triangle_interface.serialization.dart';


/// TriangleInterfaceMixin
///
/// Properties:
/// * [triangleType] 
mixin TriangleInterfaceMixin on
  $OpenApiObjectMixin {
  
            String
 get triangleType;
  
}

/// TriangleInterface
///
/// Properties:
/// * [triangleType] 
class TriangleInterface with
$OpenApiObjectMixin,


TriangleInterfaceMixin {
  @override
  
            String
 triangleType;

  AdditionalProperties<Object
?> additionalProperties;

  

  TriangleInterface.$all({
        required this.triangleType,
    required this.additionalProperties,
    
  });

  TriangleInterface({
    required  this.triangleType     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = TriangleInterfaceReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$TriangleInterfaceToMap(this);
  }
  factory TriangleInterface.fromMap(Map<String, dynamic> src) {
    return _$TriangleInterfaceFromMap(src);
  }
  static TriangleInterface? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return TriangleInterface.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$TriangleInterfaceCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory TriangleInterface.deserialize(Object? src) {
    return _$TriangleInterfaceDeserialize(src);
  }
  static TriangleInterface? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return TriangleInterface.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$TriangleInterfaceCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$TriangleInterfaceSerialize(this);
  }
}




