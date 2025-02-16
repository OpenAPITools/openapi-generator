// Model def

import 'package:petstore_api/_internal.dart';


part 'animal.reflection.dart';


/// AnimalMixin
///
/// Properties:
/// * [className] 
/// * [color] 
mixin AnimalMixin on
  $OpenApiObjectMixin {
  
            String
 get className;
UndefinedWrapper<
            String
> get color;
  
}

/// Animal
///
/// Properties:
/// * [className] 
/// * [color] 
class Animal with
$OpenApiObjectMixin,

AnimalMixin {
  @override
  
            String
 className;
  @override
  UndefinedWrapper<
            String
> color;

  AdditionalProperties<Object
?> additionalProperties;

  

  Animal.$all({
        required this.className,
    required this.color,
    required this.additionalProperties,
    
  });

  Animal({
    required  this.className     ,
  this.color = const UndefinedWrapper
    (
        
        'red'
    )
    
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = AnimalReflection.instance;
  AnimalReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Animal.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Animal clone() {
    return $reflection.clone(this);
  }
}








