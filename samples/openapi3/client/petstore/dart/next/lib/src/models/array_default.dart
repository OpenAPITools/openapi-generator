// Model def

import 'package:petstore_api/_internal.dart';


part 'array_default.reflection.dart';


/// ArrayDefaultMixin
///
/// Properties:
/// * [withDefaultEmptyBracket] 
/// * [withoutDefault] 
mixin ArrayDefaultMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
            String
>
> get withDefaultEmptyBracket;
UndefinedWrapper<
    List<
        
            String
>
> get withoutDefault;
  
}

/// ArrayDefault
///
/// Properties:
/// * [withDefaultEmptyBracket] 
/// * [withoutDefault] 
class ArrayDefault with
$OpenApiObjectMixin,

ArrayDefaultMixin {
  @override
  UndefinedWrapper<
    List<
        
            String
>
> withDefaultEmptyBracket;
  @override
  UndefinedWrapper<
    List<
        
            String
>
> withoutDefault;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayDefault.$all({
        required this.withDefaultEmptyBracket,
    required this.withoutDefault,
    required this.additionalProperties,
    
  });

  ArrayDefault({
      this.withDefaultEmptyBracket = const UndefinedWrapper
    (
        
        []
    )
    
,
  this.withoutDefault = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ArrayDefaultReflection.instance;
  ArrayDefaultReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ArrayDefault.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ArrayDefault clone() {
    return $reflection.clone(this);
  }
}












