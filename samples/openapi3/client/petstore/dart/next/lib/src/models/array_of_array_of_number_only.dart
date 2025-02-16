// Model def

import 'package:petstore_api/_internal.dart';


part 'array_of_array_of_number_only.reflection.dart';


/// ArrayOfArrayOfNumberOnlyMixin
///
/// Properties:
/// * [arrayArrayNumber] 
mixin ArrayOfArrayOfNumberOnlyMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
> get arrayArrayNumber;
  
}

/// ArrayOfArrayOfNumberOnly
///
/// Properties:
/// * [arrayArrayNumber] 
class ArrayOfArrayOfNumberOnly with
$OpenApiObjectMixin,

ArrayOfArrayOfNumberOnlyMixin {
  @override
  UndefinedWrapper<
    List<
        
    List<
        
            num
>
>
> arrayArrayNumber;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayOfArrayOfNumberOnly.$all({
        required this.arrayArrayNumber,
    required this.additionalProperties,
    
  });

  ArrayOfArrayOfNumberOnly({
      this.arrayArrayNumber = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ArrayOfArrayOfNumberOnlyReflection.instance;
  ArrayOfArrayOfNumberOnlyReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ArrayOfArrayOfNumberOnly.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ArrayOfArrayOfNumberOnly clone() {
    return $reflection.clone(this);
  }
}









