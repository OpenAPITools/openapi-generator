// Model def

import 'package:petstore_api/_internal.dart';


part '__200_response.reflection.dart';


/// Model for testing model name starting with number
///
/// Properties:
/// * [name] 
/// * [propertyClass] 
mixin $200ResponseMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get name;
UndefinedWrapper<
            String
> get propertyClass;
  
}

/// Model for testing model name starting with number
///
/// Properties:
/// * [name] 
/// * [propertyClass] 
class $200Response with
$OpenApiObjectMixin,

$200ResponseMixin {
  @override
  UndefinedWrapper<
            int
> name;
  @override
  UndefinedWrapper<
            String
> propertyClass;

  AdditionalProperties<Object
?> additionalProperties;

  

  $200Response.$all({
        required this.name,
    required this.propertyClass,
    required this.additionalProperties,
    
  });

  $200Response({
      this.name = const UndefinedWrapper
        .undefined()
,
  this.propertyClass = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = $200ResponseReflection.instance;
  $200ResponseReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory $200Response.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  $200Response clone() {
    return $reflection.clone(this);
  }
}








