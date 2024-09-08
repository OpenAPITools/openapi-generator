// Model def

import 'package:petstore_api/_internal.dart';


part 'fake_one_of_w_ith_same_erasure_get200_response.reflection.dart';


/// FakeOneOfWIthSameErasureGet200ResponseMixin
mixin FakeOneOfWIthSameErasureGet200ResponseMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
    List<
        
            String
>
> get oneOf0;
  UndefinedWrapper<
    List<
        
            int
>
> get oneOf1;
}

/// FakeOneOfWIthSameErasureGet200Response
class FakeOneOfWIthSameErasureGet200Response with
$OpenApiObjectMixin,

FakeOneOfWIthSameErasureGet200ResponseMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
    List<
        
            String
>
> oneOf0;
  
  @override
  UndefinedWrapper<
    List<
        
            int
>
> oneOf1;
  

  FakeOneOfWIthSameErasureGet200Response.$all({
        required this.additionalProperties,
    
    required this.oneOf0,
    required this.oneOf1,
  });

  FakeOneOfWIthSameErasureGet200Response({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = FakeOneOfWIthSameErasureGet200ResponseReflection.instance;
  FakeOneOfWIthSameErasureGet200ResponseReflection get $classReflection => $reflection;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length > 1) {
        // there must be AT MOST one "oneOf" schema.
        return false;
      }
      
    return super.validate();
  }

  factory FakeOneOfWIthSameErasureGet200Response.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  FakeOneOfWIthSameErasureGet200Response clone() {
    return $reflection.clone(this);
  }
}


