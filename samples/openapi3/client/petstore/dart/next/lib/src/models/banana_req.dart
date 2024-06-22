// Model def

import 'package:openapi/_internal.dart';


part 'banana_req.reflection.dart';
part 'banana_req.serialization.dart';


/// BananaReqMixin
///
/// Properties:
/// * [lengthCm] 
/// * [sweet] 
mixin BananaReqMixin on 
  $OpenApiObjectMixin {
  
            num
 get lengthCm;
UndefinedWrapper<
            bool
> get sweet;
  
}

/// BananaReq
///
/// Properties:
/// * [lengthCm] 
/// * [sweet] 
class BananaReq with
$OpenApiObjectMixin,


BananaReqMixin {
  @override
  
            num
 lengthCm;
  @override
  UndefinedWrapper<
            bool
> sweet;

  

  

  BananaReq.$all({
        required this.lengthCm,
    required this.sweet,
    
    
  });

  BananaReq({
    required  this.lengthCm     ,
  this.sweet = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = BananaReqReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$BananaReqToMap(this);
  }
  factory BananaReq.fromMap(Map<String, dynamic> src) {
    return _$BananaReqFromMap(src);
  }
  static BananaReq? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return BananaReq.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$BananaReqCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory BananaReq.deserialize(Object? src) {
    return _$BananaReqDeserialize(src);
  }
  static BananaReq? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return BananaReq.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$BananaReqCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$BananaReqSerialize(this);
  }
}




