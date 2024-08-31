// Model def

import 'package:petstore_api/_internal.dart';


part 'mixed_type_one_of_object.reflection.dart';


/// MixedTypeOneOfObjectMixin
///
/// Properties:
/// * [c] 
mixin MixedTypeOneOfObjectMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            bool
> get c;
  
  UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf
> get anyOf0;
  UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf1
> get anyOf1;
}

/// MixedTypeOneOfObject
///
/// Properties:
/// * [c] 
class MixedTypeOneOfObject with
$OpenApiObjectMixin,

MixedTypeOneOfObjectMixin {
  @override
  UndefinedWrapper<
            bool
> c;

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf
> anyOf0;
  
  @override
  UndefinedWrapper<
            MixedTypeOneOfObjectAnyOf1
> anyOf1;
  

  MixedTypeOneOfObject.$all({
        required this.c,
    required this.additionalProperties,
    
    required this.anyOf0,
    required this.anyOf1,
  });

  MixedTypeOneOfObject({
      this.c = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MixedTypeOneOfObjectReflection.instance;
  MixedTypeOneOfObjectReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory MixedTypeOneOfObject.deserialize(Object? src, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serialize(this, context);
  }
}





