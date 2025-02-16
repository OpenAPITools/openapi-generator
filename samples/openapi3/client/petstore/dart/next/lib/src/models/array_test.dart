// Model def

import 'package:petstore_api/_internal.dart';


part 'array_test.reflection.dart';


/// ArrayTestMixin
///
/// Properties:
/// * [arrayOfString] 
/// * [arrayArrayOfInteger] 
/// * [arrayArrayOfModel] 
mixin ArrayTestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
            String
>
> get arrayOfString;
UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
> get arrayArrayOfInteger;
UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
> get arrayArrayOfModel;
  
}

/// ArrayTest
///
/// Properties:
/// * [arrayOfString] 
/// * [arrayArrayOfInteger] 
/// * [arrayArrayOfModel] 
class ArrayTest with
$OpenApiObjectMixin,

ArrayTestMixin {
  @override
  UndefinedWrapper<
    List<
        
            String
>
> arrayOfString;
  @override
  UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
> arrayArrayOfInteger;
  @override
  UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
> arrayArrayOfModel;

  AdditionalProperties<Object
?> additionalProperties;

  

  ArrayTest.$all({
        required this.arrayOfString,
    required this.arrayArrayOfInteger,
    required this.arrayArrayOfModel,
    required this.additionalProperties,
    
  });

  ArrayTest({
      this.arrayOfString = const UndefinedWrapper
        .undefined()
,
  this.arrayArrayOfInteger = const UndefinedWrapper
        .undefined()
,
  this.arrayArrayOfModel = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ArrayTestReflection.instance;
  ArrayTestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ArrayTest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ArrayTest clone() {
    return $reflection.clone(this);
  }
}





















