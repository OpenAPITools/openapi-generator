// Model def

import 'package:petstore_api/_internal.dart';


part 'mixed_type_one_of_object_one_of1.reflection.dart';


/// MixedTypeOneOfObjectOneOf1Mixin
///
/// Properties:
/// * [url] 
mixin MixedTypeOneOfObjectOneOf1Mixin on
  $OpenApiObjectMixin {
  
            Uri
 get url;
  
}

/// MixedTypeOneOfObjectOneOf1
///
/// Properties:
/// * [url] 
class MixedTypeOneOfObjectOneOf1 with
$OpenApiObjectMixin,

MixedTypeOneOfObjectOneOf1Mixin {
  @override
  
            Uri
 url;

  AdditionalProperties<Object
?> additionalProperties;

  

  MixedTypeOneOfObjectOneOf1.$all({
        required this.url,
    required this.additionalProperties,
    
  });

  MixedTypeOneOfObjectOneOf1({
    required  this.url     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MixedTypeOneOfObjectOneOf1Reflection.instance;
  MixedTypeOneOfObjectOneOf1Reflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory MixedTypeOneOfObjectOneOf1.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  MixedTypeOneOfObjectOneOf1 clone() {
    return $reflection.clone(this);
  }
}





