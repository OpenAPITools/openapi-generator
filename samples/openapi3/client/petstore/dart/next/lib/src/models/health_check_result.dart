// Model def

import 'package:petstore_api/_internal.dart';


part 'health_check_result.reflection.dart';


/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
///
/// Properties:
/// * [nullableMessage] 
mixin HealthCheckResultMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
?> get nullableMessage;
  
}

/// Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
///
/// Properties:
/// * [nullableMessage] 
class HealthCheckResult with
$OpenApiObjectMixin,

HealthCheckResultMixin {
  @override
  UndefinedWrapper<
            String
?> nullableMessage;

  AdditionalProperties<Object
?> additionalProperties;

  

  HealthCheckResult.$all({
        required this.nullableMessage,
    required this.additionalProperties,
    
  });

  HealthCheckResult({
      this.nullableMessage = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = HealthCheckResultReflection.instance;
  HealthCheckResultReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory HealthCheckResult.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  HealthCheckResult clone() {
    return $reflection.clone(this);
  }
}





