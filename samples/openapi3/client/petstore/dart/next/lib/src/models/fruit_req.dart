// Model def

import 'package:petstore_api/_internal.dart';


part 'fruit_req.reflection.dart';


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
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory FruitReq.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  FruitReq clone() {
    return $reflection.clone(this);
  }
}


