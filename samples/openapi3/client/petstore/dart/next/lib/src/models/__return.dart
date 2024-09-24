// Model def

import 'package:petstore_api/_internal.dart';


part '__return.reflection.dart';


/// Model for testing reserved words
///
/// Properties:
/// * [$return] 
mixin $ReturnMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get $return;
  
}

/// Model for testing reserved words
///
/// Properties:
/// * [$return] 
class $Return with
$OpenApiObjectMixin,

$ReturnMixin {
  @override
  UndefinedWrapper<
            int
> $return;

  AdditionalProperties<Object
?> additionalProperties;

  

  $Return.$all({
        required this.$return,
    required this.additionalProperties,
    
  });

  $Return({
      this.$return = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = $ReturnReflection.instance;
  $ReturnReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory $Return.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  $Return clone() {
    return $reflection.clone(this);
  }
}





