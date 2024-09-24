// Model def

import 'package:petstore_api/_internal.dart';


part 'read_only_first.reflection.dart';


/// ReadOnlyFirstMixin
///
/// Properties:
/// * [bar] 
/// * [baz] 
mixin ReadOnlyFirstMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get bar;
UndefinedWrapper<
            String
> get baz;
  
}

/// ReadOnlyFirst
///
/// Properties:
/// * [bar] 
/// * [baz] 
class ReadOnlyFirst with
$OpenApiObjectMixin,

ReadOnlyFirstMixin {
  @override
  UndefinedWrapper<
            String
> bar;
  @override
  UndefinedWrapper<
            String
> baz;

  AdditionalProperties<Object
?> additionalProperties;

  

  ReadOnlyFirst.$all({
        required this.bar,
    required this.baz,
    required this.additionalProperties,
    
  });

  ReadOnlyFirst({
      this.bar = const UndefinedWrapper
        .undefined()
,
  this.baz = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ReadOnlyFirstReflection.instance;
  ReadOnlyFirstReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory ReadOnlyFirst.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  ReadOnlyFirst clone() {
    return $reflection.clone(this);
  }
}








