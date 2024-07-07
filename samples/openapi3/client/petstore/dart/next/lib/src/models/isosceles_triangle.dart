// Model def

import 'package:petstore_api/_internal.dart';


part 'isosceles_triangle.reflection.dart';
part 'isosceles_triangle.serialization.dart';


/// IsoscelesTriangleMixin
///
/// Properties:
mixin IsoscelesTriangleMixin on
  ShapeInterfaceMixin, TriangleInterfaceMixin, $OpenApiObjectMixin {
    
}

/// IsoscelesTriangle
///
/// Properties:
/// * [shapeType] 
/// * [triangleType] 
class IsoscelesTriangle with
$OpenApiObjectMixin,
ShapeInterfaceMixin,TriangleInterfaceMixin,
IsoscelesTriangleMixin {
  @override
  
            String
 shapeType;
  @override
  
            String
 triangleType;



  

  IsoscelesTriangle.$all({
        required this.shapeType,
    required this.triangleType,
    
    
  });

  IsoscelesTriangle({
    required  this.shapeType     ,
required  this.triangleType     ,
    
    
  });

  static const $reflection = IsoscelesTriangleReflection.instance;
  IsoscelesTriangleReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$IsoscelesTriangleToMap(this);
  }
  factory IsoscelesTriangle.fromMap(Map<String, dynamic> src) {
    return _$IsoscelesTriangleFromMap(src);
  }
  static IsoscelesTriangle? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return IsoscelesTriangle.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$IsoscelesTriangleCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory IsoscelesTriangle.deserialize(Object? src) {
    return _$IsoscelesTriangleDeserialize(src);
  }
  static IsoscelesTriangle? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return IsoscelesTriangle.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$IsoscelesTriangleCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$IsoscelesTriangleSerialize(this);
  }
}




