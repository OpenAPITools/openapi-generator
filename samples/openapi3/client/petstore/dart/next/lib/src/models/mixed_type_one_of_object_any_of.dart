// Model def

import 'package:petstore_api/_internal.dart';


part 'mixed_type_one_of_object_any_of.reflection.dart';


/// MixedTypeOneOfObjectAnyOfMixin
///
/// Properties:
/// * [a] 
mixin MixedTypeOneOfObjectAnyOfMixin on
  $OpenApiObjectMixin {
  
            int
 get a;
  
}

/// MixedTypeOneOfObjectAnyOf
///
/// Properties:
/// * [a] 
class MixedTypeOneOfObjectAnyOf with
$OpenApiObjectMixin,

MixedTypeOneOfObjectAnyOfMixin {
  @override
  
            int
 a;

  AdditionalProperties<Object
?> additionalProperties;

  

  MixedTypeOneOfObjectAnyOf.$all({
        required this.a,
    required this.additionalProperties,
    
  });

  MixedTypeOneOfObjectAnyOf({
    required  this.a     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MixedTypeOneOfObjectAnyOfReflection.instance;
  MixedTypeOneOfObjectAnyOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory MixedTypeOneOfObjectAnyOf.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  MixedTypeOneOfObjectAnyOf clone() {
    return $reflection.clone(this);
  }
}





