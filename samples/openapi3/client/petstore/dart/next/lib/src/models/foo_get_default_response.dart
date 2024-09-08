// Model def

import 'package:petstore_api/_internal.dart';


part 'foo_get_default_response.reflection.dart';


/// FooGetDefaultResponseMixin
///
/// Properties:
/// * [string] 
mixin FooGetDefaultResponseMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            Foo
> get string;
  
}

/// FooGetDefaultResponse
///
/// Properties:
/// * [string] 
class FooGetDefaultResponse with
$OpenApiObjectMixin,

FooGetDefaultResponseMixin {
  @override
  UndefinedWrapper<
            Foo
> string;

  AdditionalProperties<Object
?> additionalProperties;

  

  FooGetDefaultResponse.$all({
        required this.string,
    required this.additionalProperties,
    
  });

  FooGetDefaultResponse({
      this.string = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = FooGetDefaultResponseReflection.instance;
  FooGetDefaultResponseReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory FooGetDefaultResponse.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  FooGetDefaultResponse clone() {
    return $reflection.clone(this);
  }
}





