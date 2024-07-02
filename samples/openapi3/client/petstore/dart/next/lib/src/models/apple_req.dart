// Model def

import 'package:openapi/_internal.dart';


part 'apple_req.reflection.dart';
part 'apple_req.serialization.dart';


/// AppleReqMixin
///
/// Properties:
/// * [cultivar] 
/// * [mealy] 
mixin AppleReqMixin on
  $OpenApiObjectMixin {
  
            String
 get cultivar;
UndefinedWrapper<
            bool
> get mealy;
  
}

/// AppleReq
///
/// Properties:
/// * [cultivar] 
/// * [mealy] 
class AppleReq with
$OpenApiObjectMixin,


AppleReqMixin {
  @override
  
            String
 cultivar;
  @override
  UndefinedWrapper<
            bool
> mealy;



  

  AppleReq.$all({
        required this.cultivar,
    required this.mealy,
    
    
  });

  AppleReq({
    required  this.cultivar     ,
  this.mealy = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = AppleReqReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$AppleReqToMap(this);
  }
  factory AppleReq.fromMap(Map<String, dynamic> src) {
    return _$AppleReqFromMap(src);
  }
  static AppleReq? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return AppleReq.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$AppleReqCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory AppleReq.deserialize(Object? src) {
    return _$AppleReqDeserialize(src);
  }
  static AppleReq? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return AppleReq.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$AppleReqCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$AppleReqSerialize(this);
  }
}




