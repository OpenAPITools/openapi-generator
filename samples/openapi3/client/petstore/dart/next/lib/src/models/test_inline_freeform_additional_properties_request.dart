// Model def

import 'package:petstore_api/_internal.dart';


part 'test_inline_freeform_additional_properties_request.reflection.dart';


/// TestInlineFreeformAdditionalPropertiesRequestMixin
///
/// Properties:
/// * [someProperty] 
mixin TestInlineFreeformAdditionalPropertiesRequestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get someProperty;
  
}

/// TestInlineFreeformAdditionalPropertiesRequest
///
/// Properties:
/// * [someProperty] 
class TestInlineFreeformAdditionalPropertiesRequest with
$OpenApiObjectMixin,

TestInlineFreeformAdditionalPropertiesRequestMixin {
  @override
  UndefinedWrapper<
            String
> someProperty;

  AdditionalProperties<Object
?> additionalProperties;

  

  TestInlineFreeformAdditionalPropertiesRequest.$all({
        required this.someProperty,
    required this.additionalProperties,
    
  });

  TestInlineFreeformAdditionalPropertiesRequest({
      this.someProperty = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = TestInlineFreeformAdditionalPropertiesRequestReflection.instance;
  TestInlineFreeformAdditionalPropertiesRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory TestInlineFreeformAdditionalPropertiesRequest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  TestInlineFreeformAdditionalPropertiesRequest clone() {
    return $reflection.clone(this);
  }
}





