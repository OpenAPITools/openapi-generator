// Model def

import 'package:petstore_api/_internal.dart';


part 'fruit.reflection.dart';


/// FruitMixin
///
/// Properties:
/// * [color] 
mixin FruitMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get color;
  
  UndefinedWrapper<
            Apple
> get oneOf0;
  UndefinedWrapper<
            Banana
> get oneOf1;
}

/// Fruit
///
/// Properties:
/// * [color] 
class Fruit with
$OpenApiObjectMixin,

FruitMixin {
  @override
  UndefinedWrapper<
            String
> color;



  
  @override
  UndefinedWrapper<
            Apple
> oneOf0;
  
  @override
  UndefinedWrapper<
            Banana
> oneOf1;
  

  Fruit.$all({
        required this.color,
    
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Fruit({
      this.color = const UndefinedWrapper
        .undefined()
,
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = FruitReflection.instance;
  FruitReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory Fruit.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Fruit clone() {
    return $reflection.clone(this);
  }
}





