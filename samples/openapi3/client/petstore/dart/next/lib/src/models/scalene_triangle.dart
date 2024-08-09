// Model def

import 'package:petstore_api/_internal.dart';


part 'scalene_triangle.reflection.dart';
part 'scalene_triangle.serialization.dart';


/// ScaleneTriangleMixin
///
/// Properties:
mixin ScaleneTriangleMixin on
  ShapeInterfaceMixin, TriangleInterfaceMixin, $OpenApiObjectMixin {
    
}

/// ScaleneTriangle
///
/// Properties:
/// * [shapeType] 
/// * [triangleType] 
class ScaleneTriangle with
$OpenApiObjectMixin,
ShapeInterfaceMixin,TriangleInterfaceMixin,
ScaleneTriangleMixin {
  @override
  
            String

 shapeType;
  @override
  
            String

 triangleType;

  AdditionalProperties<Object

?> additionalProperties;

  

  ScaleneTriangle.$all({
        required this.shapeType,
    required this.triangleType,
    required this.additionalProperties,
    
  });

  ScaleneTriangle({
    required  this.shapeType     ,
required  this.triangleType     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ScaleneTriangleReflection.instance;
  ScaleneTriangleReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$ScaleneTriangleToMap(this);
  }
  factory ScaleneTriangle.fromMap(Map<String, dynamic> src) {
    return _$ScaleneTriangleFromMap(src);
  }
  static ScaleneTriangle? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ScaleneTriangle.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ScaleneTriangleCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ScaleneTriangle.deserialize(Object? src) {
    return _$ScaleneTriangleDeserialize(src);
  }
  static ScaleneTriangle? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ScaleneTriangle.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ScaleneTriangleCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ScaleneTriangleSerialize(this);
  }
}




