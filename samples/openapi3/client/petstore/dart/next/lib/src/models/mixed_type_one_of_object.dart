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
            MixedTypeOneOfObjectOneOf
> get oneOf0;
  UndefinedWrapper<
            MixedTypeOneOfObjectOneOf1
> get oneOf1;
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
            MixedTypeOneOfObjectOneOf
> oneOf0;
  
  @override
  UndefinedWrapper<
            MixedTypeOneOfObjectOneOf1
> oneOf1;
  
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
    
    required this.oneOf0,
    required this.oneOf1,
    required this.anyOf0,
    required this.anyOf1,
  });

  MixedTypeOneOfObject({
      this.c = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = MixedTypeOneOfObjectReflection.instance;
  MixedTypeOneOfObjectReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory MixedTypeOneOfObject.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  MixedTypeOneOfObject clone() {
    return $reflection.clone(this);
  }
}





