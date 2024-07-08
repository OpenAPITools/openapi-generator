// Model def

import 'package:petstore_api/_internal.dart';


part 'nullable_shape.reflection.dart';
part 'nullable_shape.serialization.dart';


/// The value may be a shape or the 'null' value. The 'nullable' attribute was introduced in OAS schema >= 3.0 and has been deprecated in OAS schema >= 3.1.
mixin NullableShapeMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            Triangle
> get oneOf0;
  UndefinedWrapper<
            Quadrilateral
> get oneOf1;
}

/// The value may be a shape or the 'null' value. The 'nullable' attribute was introduced in OAS schema >= 3.0 and has been deprecated in OAS schema >= 3.1.
class NullableShape with
$OpenApiObjectMixin,

NullableShapeMixin {

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
  

  NullableShape.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  NullableShape({
        this.additionalProperties = const AdditionalProperties(),
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = NullableShapeReflection.instance;
  NullableShapeReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$NullableShapeToMap(this);
  }
  factory NullableShape.fromMap(Map<String, dynamic> src) {
    return _$NullableShapeFromMap(src);
  }
  static NullableShape? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return NullableShape.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$NullableShapeCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory NullableShape.deserialize(Object? src) {
    return _$NullableShapeDeserialize(src);
  }
  static NullableShape? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return NullableShape.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$NullableShapeCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$NullableShapeSerialize(this);
  }
}




