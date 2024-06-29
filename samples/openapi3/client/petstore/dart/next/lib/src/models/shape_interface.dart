// Model def

import 'package:openapi/_internal.dart';


part 'shape_interface.reflection.dart';
part 'shape_interface.serialization.dart';


/// ShapeInterfaceMixin
///
/// Properties:
/// * [shapeType] 
mixin ShapeInterfaceMixin on
  $OpenApiObjectMixin {
  
            String
 get shapeType;
  
}

/// ShapeInterface
///
/// Properties:
/// * [shapeType] 
class ShapeInterface with
$OpenApiObjectMixin,


ShapeInterfaceMixin {
  @override
  
            String
 shapeType;

  AdditionalProperties<Object
?> additionalProperties;

  

  ShapeInterface.$all({
        required this.shapeType,
    required this.additionalProperties,
    
  });

  ShapeInterface({
    required  this.shapeType     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ShapeInterfaceReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ShapeInterfaceToMap(this);
  }
  factory ShapeInterface.fromMap(Map<String, dynamic> src) {
    return _$ShapeInterfaceFromMap(src);
  }
  static ShapeInterface? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ShapeInterface.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ShapeInterfaceCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ShapeInterface.deserialize(Object? src) {
    return _$ShapeInterfaceDeserialize(src);
  }
  static ShapeInterface? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ShapeInterface.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ShapeInterfaceCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ShapeInterfaceSerialize(this);
  }
}




