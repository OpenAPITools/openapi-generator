// Model def

import 'package:petstore_api/_internal.dart';


part 'name.reflection.dart';


/// Model for testing model name same as property name
///
/// Properties:
/// * [name] 
/// * [snakeCase] 
/// * [property] 
/// * [$123number] 
mixin NameMixin on
  $OpenApiObjectMixin {
  
            int
 get name;
UndefinedWrapper<
            int
> get snakeCase;
UndefinedWrapper<
            String
> get property;
UndefinedWrapper<
            int
> get $123number;
  
}

/// Model for testing model name same as property name
///
/// Properties:
/// * [name] 
/// * [snakeCase] 
/// * [property] 
/// * [$123number] 
class Name with
$OpenApiObjectMixin,

NameMixin {
  @override
  
            int
 name;
  @override
  UndefinedWrapper<
            int
> snakeCase;
  @override
  UndefinedWrapper<
            String
> property;
  @override
  UndefinedWrapper<
            int
> $123number;

  AdditionalProperties<Object
?> additionalProperties;

  

  Name.$all({
        required this.name,
    required this.snakeCase,
    required this.property,
    required this.$123number,
    required this.additionalProperties,
    
  });

  Name({
    required  this.name     ,
  this.snakeCase = const UndefinedWrapper
        .undefined()
,
  this.property = const UndefinedWrapper
        .undefined()
,
  this.$123number = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = NameReflection.instance;
  NameReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Name.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Name clone() {
    return $reflection.clone(this);
  }
}














