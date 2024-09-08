// Model def

import 'package:petstore_api/_internal.dart';


part 'deprecated_object.reflection.dart';


/// DeprecatedObjectMixin
///
/// Properties:
/// * [name] 
@Deprecated('DeprecatedObjectMixin has been deprecated')
mixin DeprecatedObjectMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get name;
  
}

/// DeprecatedObject
///
/// Properties:
/// * [name] 
@Deprecated('DeprecatedObjectMixin has been deprecated')
class DeprecatedObject with
$OpenApiObjectMixin,

DeprecatedObjectMixin {
  @override
  UndefinedWrapper<
            String
> name;

  AdditionalProperties<Object
?> additionalProperties;

  

  DeprecatedObject.$all({
        required this.name,
    required this.additionalProperties,
    
  });

  DeprecatedObject({
      this.name = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = DeprecatedObjectReflection.instance;
  DeprecatedObjectReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory DeprecatedObject.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  DeprecatedObject clone() {
    return $reflection.clone(this);
  }
}





