// Model def

import 'package:petstore_api/_internal.dart';


part 'gm_fruit.reflection.dart';


/// GmFruitMixin
///
/// Properties:
/// * [color] 
mixin GmFruitMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get color;
  
  UndefinedWrapper<
            Apple
> get anyOf0;
  UndefinedWrapper<
            Banana
> get anyOf1;
}

/// GmFruit
///
/// Properties:
/// * [color] 
class GmFruit with
$OpenApiObjectMixin,

GmFruitMixin {
  @override
  UndefinedWrapper<
            String
> color;



  
  @override
  UndefinedWrapper<
            Apple
> anyOf0;
  
  @override
  UndefinedWrapper<
            Banana
> anyOf1;
  

  GmFruit.$all({
        required this.color,
    
    
    required this.anyOf0,
    required this.anyOf1,
  });

  GmFruit({
      this.color = const UndefinedWrapper
        .undefined()
,
    
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  });

  static const $reflection = GmFruitReflection.instance;
  GmFruitReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory GmFruit.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  GmFruit clone() {
    return $reflection.clone(this);
  }
}





