// Model def

import 'package:openapi/_internal.dart';


part 'scalar.reflection.dart';
part 'scalar.serialization.dart';


/// Values of scalar type
mixin ScalarMixin on 
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            String
> get oneOf0;
  UndefinedWrapper<
            num
> get oneOf1;
  UndefinedWrapper<
            bool
> get oneOf2;
}

/// Values of scalar type
class Scalar with
$OpenApiObjectMixin,


ScalarMixin {

  

  
  @override
  UndefinedWrapper<
            String
> oneOf0;
  
  @override
  UndefinedWrapper<
            num
> oneOf1;
  
  @override
  UndefinedWrapper<
            bool
> oneOf2;
  

  Scalar.$all({
        
    
    required this.oneOf0,
    required this.oneOf1,
    required this.oneOf2,
  });

  Scalar({
        
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
    this.oneOf2 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = ScalarReflection.instance;

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
    return _$ScalarToMap(this);
  }
  factory Scalar.fromMap(Map<String, dynamic> src) {
    return _$ScalarFromMap(src);
  }
  static Scalar? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Scalar.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ScalarCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Scalar.deserialize(Object? src) {
    return _$ScalarDeserialize(src);
  }
  static Scalar? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Scalar.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ScalarCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ScalarSerialize(this);
  }
}




