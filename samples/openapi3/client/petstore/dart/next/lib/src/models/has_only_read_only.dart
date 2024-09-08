// Model def

import 'package:petstore_api/_internal.dart';


part 'has_only_read_only.reflection.dart';


/// HasOnlyReadOnlyMixin
///
/// Properties:
/// * [bar] 
/// * [foo] 
mixin HasOnlyReadOnlyMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get bar;
UndefinedWrapper<
            String
> get foo;
  
}

/// HasOnlyReadOnly
///
/// Properties:
/// * [bar] 
/// * [foo] 
class HasOnlyReadOnly with
$OpenApiObjectMixin,

HasOnlyReadOnlyMixin {
  @override
  UndefinedWrapper<
            String
> bar;
  @override
  UndefinedWrapper<
            String
> foo;

  AdditionalProperties<Object
?> additionalProperties;

  

  HasOnlyReadOnly.$all({
        required this.bar,
    required this.foo,
    required this.additionalProperties,
    
  });

  HasOnlyReadOnly({
      this.bar = const UndefinedWrapper
        .undefined()
,
  this.foo = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = HasOnlyReadOnlyReflection.instance;
  HasOnlyReadOnlyReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory HasOnlyReadOnly.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  HasOnlyReadOnly clone() {
    return $reflection.clone(this);
  }
}








