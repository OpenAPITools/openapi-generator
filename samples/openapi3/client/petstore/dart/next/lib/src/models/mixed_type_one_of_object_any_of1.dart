// Model def

import 'package:petstore_api/_internal.dart';


part 'mixed_type_one_of_object_any_of1.reflection.dart';


/// MixedTypeOneOfObjectAnyOf1Mixin
///
/// Properties:
/// * [b] 
mixin MixedTypeOneOfObjectAnyOf1Mixin on
  $OpenApiObjectMixin {
  
            num
 get b;
  
}

/// MixedTypeOneOfObjectAnyOf1
///
/// Properties:
/// * [b] 
class MixedTypeOneOfObjectAnyOf1 with
$OpenApiObjectMixin,

MixedTypeOneOfObjectAnyOf1Mixin {
  @override
  
            num
 b;

  AdditionalProperties<Object
?> additionalProperties;

  

  MixedTypeOneOfObjectAnyOf1.$all({
        required this.b,
    required this.additionalProperties,
    
  });

  MixedTypeOneOfObjectAnyOf1({
    required  this.b     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MixedTypeOneOfObjectAnyOf1Reflection.instance;
  MixedTypeOneOfObjectAnyOf1Reflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory MixedTypeOneOfObjectAnyOf1.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  MixedTypeOneOfObjectAnyOf1 clone() {
    return $reflection.clone(this);
  }
}





