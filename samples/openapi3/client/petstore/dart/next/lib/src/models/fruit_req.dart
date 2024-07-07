// Model def

import 'package:petstore_api/_internal.dart';


part 'fruit_req.reflection.dart';
part 'fruit_req.serialization.dart';


/// FruitReqMixin
///
/// Properties:
mixin FruitReqMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            AppleReq
> get oneOf0;
  UndefinedWrapper<
            BananaReq
> get oneOf1;
}

/// FruitReq
///
/// Properties:
class FruitReq with
$OpenApiObjectMixin,

FruitReqMixin {



  
  @override
  UndefinedWrapper<
            AppleReq
> oneOf0;
  
  @override
  UndefinedWrapper<
            BananaReq
> oneOf1;
  

  FruitReq.$all({
        
    
    required this.oneOf0,
    required this.oneOf1,
  });

  FruitReq({
        
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = FruitReqReflection.instance;
  FruitReqReflection get $classReflection => $reflection;

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
    return _$FruitReqToMap(this);
  }
  factory FruitReq.fromMap(Map<String, dynamic> src) {
    return _$FruitReqFromMap(src);
  }
  static FruitReq? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return FruitReq.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FruitReqCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory FruitReq.deserialize(Object? src) {
    return _$FruitReqDeserialize(src);
  }
  static FruitReq? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return FruitReq.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FruitReqCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$FruitReqSerialize(this);
  }
}




