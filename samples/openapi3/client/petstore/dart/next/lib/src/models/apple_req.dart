// Model def

import 'package:petstore_api/_internal.dart';


part 'apple_req.reflection.dart';


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
  AppleReqReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory AppleReq.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  AppleReq clone() {
    return $reflection.clone(this);
  }
}








