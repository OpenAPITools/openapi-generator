// Model def

import 'package:openapi/_internal.dart';


part 'shape_or_null.reflection.dart';
part 'shape_or_null.serialization.dart';


/// The value may be a shape or the 'null' value. This is introduced in OAS schema >= 3.1.
mixin ShapeOrNullMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            Triangle
> get oneOf0;
  UndefinedWrapper<
            Quadrilateral
> get oneOf1;
}

/// The value may be a shape or the 'null' value. This is introduced in OAS schema >= 3.1.
class ShapeOrNull with
$OpenApiObjectMixin,


ShapeOrNullMixin {

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
  

  ShapeOrNull.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  ShapeOrNull({
        this.additionalProperties = const AdditionalProperties(),
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = ShapeOrNullReflection.instance;

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
    return _$ShapeOrNullToMap(this);
  }
  factory ShapeOrNull.fromMap(Map<String, dynamic> src) {
    return _$ShapeOrNullFromMap(src);
  }
  static ShapeOrNull? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ShapeOrNull.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ShapeOrNullCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ShapeOrNull.deserialize(Object? src) {
    return _$ShapeOrNullDeserialize(src);
  }
  static ShapeOrNull? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ShapeOrNull.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ShapeOrNullCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ShapeOrNullSerialize(this);
  }
}




