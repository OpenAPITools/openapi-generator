// Model def

import 'package:petstore_api/_internal.dart';


part 'mixed_type_one_of_object_one_of.reflection.dart';


/// MixedTypeOneOfObjectOneOfMixin
///
/// Properties:
/// * [content] 
mixin MixedTypeOneOfObjectOneOfMixin on
  $OpenApiObjectMixin {
  
            String
 get content;
  
}

/// MixedTypeOneOfObjectOneOf
///
/// Properties:
/// * [content] 
class MixedTypeOneOfObjectOneOf with
$OpenApiObjectMixin,

MixedTypeOneOfObjectOneOfMixin {
  @override
  
            String
 content;

  AdditionalProperties<Object
?> additionalProperties;

  

  MixedTypeOneOfObjectOneOf.$all({
        required this.content,
    required this.additionalProperties,
    
  });

  MixedTypeOneOfObjectOneOf({
    required  this.content     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MixedTypeOneOfObjectOneOfReflection.instance;
  MixedTypeOneOfObjectOneOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory MixedTypeOneOfObjectOneOf.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  MixedTypeOneOfObjectOneOf clone() {
    return $reflection.clone(this);
  }
}





