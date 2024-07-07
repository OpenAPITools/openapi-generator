// Model def

import 'package:petstore_api/_internal.dart';


part 'equilateral_triangle.reflection.dart';
part 'equilateral_triangle.serialization.dart';


/// EquilateralTriangleMixin
///
/// Properties:
mixin EquilateralTriangleMixin on
  ShapeInterfaceMixin, TriangleInterfaceMixin, $OpenApiObjectMixin {
    
}

/// EquilateralTriangle
///
/// Properties:
/// * [shapeType] 
/// * [triangleType] 
class EquilateralTriangle with
$OpenApiObjectMixin,
ShapeInterfaceMixin,TriangleInterfaceMixin,
EquilateralTriangleMixin {
  @override
  
            String
 shapeType;
  @override
  
            String
 triangleType;

  AdditionalProperties<Object
?> additionalProperties;

  

  EquilateralTriangle.$all({
        required this.shapeType,
    required this.triangleType,
    required this.additionalProperties,
    
  });

  EquilateralTriangle({
    required  this.shapeType     ,
required  this.triangleType     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = EquilateralTriangleReflection.instance;
  EquilateralTriangleReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$EquilateralTriangleToMap(this);
  }
  factory EquilateralTriangle.fromMap(Map<String, dynamic> src) {
    return _$EquilateralTriangleFromMap(src);
  }
  static EquilateralTriangle? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return EquilateralTriangle.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$EquilateralTriangleCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory EquilateralTriangle.deserialize(Object? src) {
    return _$EquilateralTriangleDeserialize(src);
  }
  static EquilateralTriangle? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return EquilateralTriangle.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$EquilateralTriangleCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$EquilateralTriangleSerialize(this);
  }
}




