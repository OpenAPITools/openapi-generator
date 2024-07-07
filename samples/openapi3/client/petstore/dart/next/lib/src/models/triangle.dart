// Model def

import 'package:petstore_api/_internal.dart';


part 'triangle.reflection.dart';
part 'triangle.serialization.dart';


/// TriangleMixin
///
/// Properties:
mixin TriangleMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            EquilateralTriangle
> get oneOf0;
  UndefinedWrapper<
            IsoscelesTriangle
> get oneOf1;
  UndefinedWrapper<
            ScaleneTriangle
> get oneOf2;
}

/// Triangle
///
/// Properties:
class Triangle with
$OpenApiObjectMixin,

TriangleMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            EquilateralTriangle
> oneOf0;
  
  @override
  UndefinedWrapper<
            IsoscelesTriangle
> oneOf1;
  
  @override
  UndefinedWrapper<
            ScaleneTriangle
> oneOf2;
  

  Triangle.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
    required this.oneOf2,
  });

  Triangle({
        this.additionalProperties = const AdditionalProperties(),
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
    this.oneOf2 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = TriangleReflection.instance;
  TriangleReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,oneOf2,].where((e) => e.isDefined).take(2);
      if (oneOfs.length != 1) {
        // there must be EXACTLY one "oneOf" schema.
        return false;
      }
      
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$TriangleToMap(this);
  }
  factory Triangle.fromMap(Map<String, dynamic> src) {
    return _$TriangleFromMap(src);
  }
  static Triangle? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Triangle.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$TriangleCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Triangle.deserialize(Object? src) {
    return _$TriangleDeserialize(src);
  }
  static Triangle? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Triangle.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$TriangleCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$TriangleSerialize(this);
  }
}




