// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of_all_of_attributes.reflection.dart';


/// AllOfModelArrayAnyOfAllOfAttributesMixin
///
/// Properties:
/// * [C] 
mixin AllOfModelArrayAnyOfAllOfAttributesMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
> get C;
  
}

/// AllOfModelArrayAnyOfAllOfAttributes
///
/// Properties:
/// * [C] 
class AllOfModelArrayAnyOfAllOfAttributes with
$OpenApiObjectMixin,

AllOfModelArrayAnyOfAllOfAttributesMixin {
  @override
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
> C;

  AdditionalProperties<Object
?> additionalProperties;

  

  AllOfModelArrayAnyOfAllOfAttributes.$all({
        required this.C,
    required this.additionalProperties,
    
  });

  AllOfModelArrayAnyOfAllOfAttributes({
      this.C = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = AllOfModelArrayAnyOfAllOfAttributesReflection.instance;
  AllOfModelArrayAnyOfAllOfAttributesReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory AllOfModelArrayAnyOfAllOfAttributes.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  AllOfModelArrayAnyOfAllOfAttributes clone() {
    return $reflection.clone(this);
  }
}





