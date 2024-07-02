// Model def

import 'package:openapi/_internal.dart';


part 'shape.reflection.dart';
part 'shape.serialization.dart';


/// ShapeMixin
mixin ShapeMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            Triangle
> get oneOf0;
  UndefinedWrapper<
            Quadrilateral
> get oneOf1;
}

/// Shape
class Shape with
$OpenApiObjectMixin,


ShapeMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            Triangle
> oneOf0;
  
  @override
  UndefinedWrapper<
            Quadrilateral
> oneOf1;
  

  Shape.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Shape({
        this.additionalProperties = const AdditionalProperties(),
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = ShapeReflection.instance;

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
    return _$ShapeToMap(this);
  }
  factory Shape.fromMap(Map<String, dynamic> src) {
    return _$ShapeFromMap(src);
  }
  static Shape? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Shape.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ShapeCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Shape.deserialize(Object? src) {
    return _$ShapeDeserialize(src);
  }
  static Shape? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Shape.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ShapeCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ShapeSerialize(this);
  }
}




