// Model def

import 'package:petstore_api/_internal.dart';


part 'fake_any_of_w_ith_same_erasure_get200_response.reflection.dart';


/// FakeAnyOfWIthSameErasureGet200ResponseMixin
mixin FakeAnyOfWIthSameErasureGet200ResponseMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
    List<
        
            String
>
> get anyOf0;
  UndefinedWrapper<
    List<
        
            int
>
> get anyOf1;
}

/// FakeAnyOfWIthSameErasureGet200Response
class FakeAnyOfWIthSameErasureGet200Response with
$OpenApiObjectMixin,

FakeAnyOfWIthSameErasureGet200ResponseMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
    List<
        
            String
>
> anyOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            int
>
> anyOf1;
  

  FakeAnyOfWIthSameErasureGet200Response.$all({
        required this.additionalProperties,
    
    required this.anyOf0,
    required this.anyOf1,
  });

  FakeAnyOfWIthSameErasureGet200Response({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = FakeAnyOfWIthSameErasureGet200ResponseReflection.instance;
  FakeAnyOfWIthSameErasureGet200ResponseReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory FakeAnyOfWIthSameErasureGet200Response.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  FakeAnyOfWIthSameErasureGet200Response clone() {
    return $reflection.clone(this);
  }
}


