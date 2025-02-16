// Model def

import 'package:petstore_api/_internal.dart';


part 'property_name_collision.reflection.dart';


/// PropertyNameCollisionMixin
///
/// Properties:
/// * [$type] 
/// * [type] 
/// * [type$] 
mixin PropertyNameCollisionMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get $type;
UndefinedWrapper<
            String
> get type;
UndefinedWrapper<
            String
> get type$;
  
}

/// PropertyNameCollision
///
/// Properties:
/// * [$type] 
/// * [type] 
/// * [type$] 
class PropertyNameCollision with
$OpenApiObjectMixin,

PropertyNameCollisionMixin {
  @override
  UndefinedWrapper<
            String
> $type;
  @override
  UndefinedWrapper<
            String
> type;
  @override
  UndefinedWrapper<
            String
> type$;

  AdditionalProperties<Object
?> additionalProperties;

  

  PropertyNameCollision.$all({
        required this.$type,
    required this.type,
    required this.type$,
    required this.additionalProperties,
    
  });

  PropertyNameCollision({
      this.$type = const UndefinedWrapper
        .undefined()
,
  this.type = const UndefinedWrapper
        .undefined()
,
  this.type$ = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PropertyNameCollisionReflection.instance;
  PropertyNameCollisionReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory PropertyNameCollision.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  PropertyNameCollision clone() {
    return $reflection.clone(this);
  }
}











