// Model def

import 'package:openapi/_internal.dart';


part 'quadrilateral.reflection.dart';
part 'quadrilateral.serialization.dart';


/// QuadrilateralMixin
///
/// Properties:
mixin QuadrilateralMixin on 
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            SimpleQuadrilateral
> get oneOf0;
  UndefinedWrapper<
            ComplexQuadrilateral
> get oneOf1;
}

/// Quadrilateral
///
/// Properties:
class Quadrilateral with
$OpenApiObjectMixin,


QuadrilateralMixin {

  

  
  @override
  UndefinedWrapper<
            SimpleQuadrilateral
> oneOf0;
  
  @override
  UndefinedWrapper<
            ComplexQuadrilateral
> oneOf1;
  

  Quadrilateral.$all({
        
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Quadrilateral({
        
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = QuadrilateralReflection.instance;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length != 1) {
        // there must be EXACTLY one "oneOf" schema.
        return false;
      }
      
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$QuadrilateralToMap(this);
  }
  factory Quadrilateral.fromMap(Map<String, dynamic> src) {
    return _$QuadrilateralFromMap(src);
  }
  static Quadrilateral? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Quadrilateral.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$QuadrilateralCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Quadrilateral.deserialize(Object? src) {
    return _$QuadrilateralDeserialize(src);
  }
  static Quadrilateral? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Quadrilateral.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$QuadrilateralCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$QuadrilateralSerialize(this);
  }
}




