// Model def

import 'package:petstore_api/_internal.dart';


part 'dog.reflection.dart';


/// DogMixin
///
/// Properties:
/// * [breed] 
mixin DogMixin on
  AnimalMixin, $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get breed;
  
}

/// Dog
///
/// Properties:
/// * [color] 
/// * [breed] 
/// * [className] 
class Dog with
$OpenApiObjectMixin,
AnimalMixin,
DogMixin {
  @override
  UndefinedWrapper<
            String
> color;
  @override
  UndefinedWrapper<
            String
> breed;
  @override
  
            String
 className;

  AdditionalProperties<Object
?> additionalProperties;

  

  Dog.$all({
        required this.color,
    required this.breed,
    required this.className,
    required this.additionalProperties,
    
  });

  Dog({
      this.color = const UndefinedWrapper
    (
        
        'red'
    )
    
,
  this.breed = const UndefinedWrapper
        .undefined()
,
required  this.className     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = DogReflection.instance;
  DogReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory Dog.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Dog clone() {
    return $reflection.clone(this);
  }
}





