// Model def

import 'package:petstore_api/_internal.dart';


part 'foo.reflection.dart';


/// FooMixin
///
/// Properties:
/// * [bar] 
mixin FooMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get bar;
  
}

/// Foo
///
/// Properties:
/// * [bar] 
class Foo with
$OpenApiObjectMixin,

FooMixin {
  @override
  UndefinedWrapper<
            String
> bar;

  AdditionalProperties<Object
?> additionalProperties;

  

  Foo.$all({
        required this.bar,
    required this.additionalProperties,
    
  });

  Foo({
      this.bar = const UndefinedWrapper
    (
        
        'bar'
    )
    
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = FooReflection.instance;
  FooReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Foo.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Foo clone() {
    return $reflection.clone(this);
  }
}





