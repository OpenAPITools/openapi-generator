// Model def

import 'package:petstore_api/_internal.dart';


part '__list.reflection.dart';


/// $ListMixin
///
/// Properties:
/// * [$123list] 
mixin $ListMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get $123list;
  
}

/// $List
///
/// Properties:
/// * [$123list] 
class $List with
$OpenApiObjectMixin,

$ListMixin {
  @override
  UndefinedWrapper<
            String
> $123list;

  AdditionalProperties<Object
?> additionalProperties;

  

  $List.$all({
        required this.$123list,
    required this.additionalProperties,
    
  });

  $List({
      this.$123list = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = $ListReflection.instance;
  $ListReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory $List.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  $List clone() {
    return $reflection.clone(this);
  }
}





