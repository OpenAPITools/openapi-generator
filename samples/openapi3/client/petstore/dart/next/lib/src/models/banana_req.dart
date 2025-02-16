// Model def

import 'package:petstore_api/_internal.dart';


part 'banana_req.reflection.dart';


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
  BananaReqReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory BananaReq.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  BananaReq clone() {
    return $reflection.clone(this);
  }
}








