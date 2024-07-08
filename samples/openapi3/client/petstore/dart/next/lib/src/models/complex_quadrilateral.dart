// Model def

import 'package:petstore_api/_internal.dart';


part 'complex_quadrilateral.reflection.dart';
part 'complex_quadrilateral.serialization.dart';


/// ComplexQuadrilateralMixin
///
/// Properties:
mixin ComplexQuadrilateralMixin on
  QuadrilateralInterfaceMixin, ShapeInterfaceMixin, $OpenApiObjectMixin {
    
}

/// ComplexQuadrilateral
///
/// Properties:
/// * [quadrilateralType] 
/// * [shapeType] 
class ComplexQuadrilateral with
$OpenApiObjectMixin,
QuadrilateralInterfaceMixin,ShapeInterfaceMixin,
ComplexQuadrilateralMixin {
  @override
  
            String

 quadrilateralType;
  @override
  
            String

 shapeType;

  AdditionalProperties<Object

?> additionalProperties;

  

  ComplexQuadrilateral.$all({
        required this.quadrilateralType,
    required this.shapeType,
    required this.additionalProperties,
    
  });

  ComplexQuadrilateral({
    required  this.quadrilateralType     ,
required  this.shapeType     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ComplexQuadrilateralReflection.instance;
  ComplexQuadrilateralReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$ComplexQuadrilateralToMap(this);
  }
  factory ComplexQuadrilateral.fromMap(Map<String, dynamic> src) {
    return _$ComplexQuadrilateralFromMap(src);
  }
  static ComplexQuadrilateral? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ComplexQuadrilateral.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ComplexQuadrilateralCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ComplexQuadrilateral.deserialize(Object? src) {
    return _$ComplexQuadrilateralDeserialize(src);
  }
  static ComplexQuadrilateral? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ComplexQuadrilateral.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ComplexQuadrilateralCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ComplexQuadrilateralSerialize(this);
  }
}




