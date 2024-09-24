// Model def

import 'package:petstore_api/_internal.dart';


part 'mammal.reflection.dart';


/// MammalMixin
///
/// Properties:
/// * [className] 
mixin MammalMixin on
  $OpenApiObjectMixin {
  
            String
 get className;
  
  UndefinedWrapper<
            Whale
> get oneOf0;
  UndefinedWrapper<
            Zebra
> get oneOf1;
  UndefinedWrapper<
            Pig
> get oneOf2;
}

/// Mammal
///
/// Properties:
/// * [className] 
class Mammal with
$OpenApiObjectMixin,

MammalMixin {
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            Whale
> oneOf0;
  
  @override
  UndefinedWrapper<
            Zebra
> oneOf1;
  
  @override
  UndefinedWrapper<
            Pig
> oneOf2;
  

  Mammal.$all({
        required this.className,
    required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
    required this.oneOf2,
  });

  Mammal({
    required  this.className     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
    this.oneOf2 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MammalReflection.instance;
  MammalReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,oneOf2,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory Mammal.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Mammal clone() {
    return $reflection.clone(this);
  }
}





