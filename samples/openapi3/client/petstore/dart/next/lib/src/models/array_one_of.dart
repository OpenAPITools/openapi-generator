// Model def

import 'package:petstore_api/_internal.dart';


part 'array_one_of.reflection.dart';


/// ArrayOneOfMixin
mixin ArrayOneOfMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            int
> get oneOf0;
  UndefinedWrapper<
    List<
        
            String
>
> get oneOf1;
}

/// ArrayOneOf
class ArrayOneOf with
$OpenApiObjectMixin,

ArrayOneOfMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            int
> oneOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            String
>
> oneOf1;
  

  ArrayOneOf.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  ArrayOneOf({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ArrayOneOfReflection.instance;
  ArrayOneOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory ArrayOneOf.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ArrayOneOf clone() {
    return $reflection.clone(this);
  }
}


