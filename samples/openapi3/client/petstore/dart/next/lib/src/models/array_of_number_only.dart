// Model def

import 'package:petstore_api/_internal.dart';


part 'array_of_number_only.reflection.dart';


/// ArrayOfNumberOnlyMixin
///
/// Properties:
/// * [arrayNumber] 
mixin ArrayOfNumberOnlyMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
            num
>
> get arrayNumber;
  
}

/// ArrayOfNumberOnly
///
/// Properties:
/// * [arrayNumber] 
class ArrayOfNumberOnly with
$OpenApiObjectMixin,

ArrayOfNumberOnlyMixin {
  @override
  UndefinedWrapper<
    List<
        
            num
>
> arrayNumber;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayOfNumberOnly.$all({
        required this.arrayNumber,
    required this.additionalProperties,
    
  });

  ArrayOfNumberOnly({
      this.arrayNumber = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ArrayOfNumberOnlyReflection.instance;
  ArrayOfNumberOnlyReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ArrayOfNumberOnly.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ArrayOfNumberOnly clone() {
    return $reflection.clone(this);
  }
}







