// Model def

import 'package:petstore_api/_internal.dart';


part 'simple_quadrilateral.reflection.dart';
part 'simple_quadrilateral.serialization.dart';


/// SimpleQuadrilateralMixin
///
/// Properties:
mixin SimpleQuadrilateralMixin on
  QuadrilateralInterfaceMixin, ShapeInterfaceMixin, $OpenApiObjectMixin {
    
}

/// SimpleQuadrilateral
///
/// Properties:
/// * [quadrilateralType] 
/// * [shapeType] 
class SimpleQuadrilateral with
$OpenApiObjectMixin,
QuadrilateralInterfaceMixin,ShapeInterfaceMixin,
SimpleQuadrilateralMixin {
  @override
  
            String
 quadrilateralType;
  @override
  
            String
 shapeType;

  AdditionalProperties<Object
?> additionalProperties;

  

  SimpleQuadrilateral.$all({
        required this.quadrilateralType,
    required this.shapeType,
    required this.additionalProperties,
    
  });

  SimpleQuadrilateral({
    required  this.quadrilateralType     ,
required  this.shapeType     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = SimpleQuadrilateralReflection.instance;
  SimpleQuadrilateralReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$SimpleQuadrilateralToMap(this);
  }
  factory SimpleQuadrilateral.fromMap(Map<String, dynamic> src) {
    return _$SimpleQuadrilateralFromMap(src);
  }
  static SimpleQuadrilateral? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return SimpleQuadrilateral.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$SimpleQuadrilateralCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory SimpleQuadrilateral.deserialize(Object? src) {
    return _$SimpleQuadrilateralDeserialize(src);
  }
  static SimpleQuadrilateral? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return SimpleQuadrilateral.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$SimpleQuadrilateralCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$SimpleQuadrilateralSerialize(this);
  }
}




