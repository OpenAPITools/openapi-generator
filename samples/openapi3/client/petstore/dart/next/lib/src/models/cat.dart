// Model def

import 'package:petstore_api/_internal.dart';


part 'cat.reflection.dart';


/// CatMixin
///
/// Properties:
/// * [declawed] 
mixin CatMixin on
  AnimalMixin, $OpenApiObjectMixin {
  UndefinedWrapper<
            bool
> get declawed;
  
}

/// Cat
///
/// Properties:
/// * [color] 
/// * [declawed] 
/// * [className] 
class Cat with
$OpenApiObjectMixin,
AnimalMixin,
CatMixin {
  @override
  UndefinedWrapper<
            String
> color;
  @override
  UndefinedWrapper<
            bool
> declawed;
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  

  Cat.$all({
        required this.color,
    required this.declawed,
    required this.className,
    required this.additionalProperties,
    
  });

  Cat({
      this.color = const UndefinedWrapper
    (
        
        'red'
    )
    
,
  this.declawed = const UndefinedWrapper
        .undefined()
,
required  this.className     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = CatReflection.instance;
  CatReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory Cat.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Cat clone() {
    return $reflection.clone(this);
  }
}





