// Model def

import 'package:petstore_api/_internal.dart';


part 'variable.reflection.dart';


/// Value object
///
/// Properties:
/// * [name] 
/// * [value] 
mixin VariableMixin on
  $OpenApiObjectMixin {
  
            String
 get name;

            Value
 get value;
  
}

/// Value object
///
/// Properties:
/// * [name] 
/// * [value] 
class Variable with
$OpenApiObjectMixin,

VariableMixin {
  @override
  
            String
 name;
  @override
  
            Value
 value;

  AdditionalProperties<Object
?> additionalProperties;

  

  Variable.$all({
        required this.name,
    required this.value,
    required this.additionalProperties,
    
  });

  Variable({
    required  this.name     ,
required  this.value     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = VariableReflection.instance;
  VariableReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Variable.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Variable clone() {
    return $reflection.clone(this);
  }
}








