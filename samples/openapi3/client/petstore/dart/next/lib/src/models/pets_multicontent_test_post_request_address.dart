// Model def

import 'package:petstore_api/_internal.dart';


part 'pets_multicontent_test_post_request_address.reflection.dart';


/// PetsMulticontentTestPostRequestAddressMixin
///
/// Properties:
/// * [street] 
/// * [city] 
mixin PetsMulticontentTestPostRequestAddressMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get street;
UndefinedWrapper<
            String
> get city;
  
}

/// PetsMulticontentTestPostRequestAddress
///
/// Properties:
/// * [street] 
/// * [city] 
class PetsMulticontentTestPostRequestAddress with
$OpenApiObjectMixin,

PetsMulticontentTestPostRequestAddressMixin {
  @override
  UndefinedWrapper<
            String
> street;
  @override
  UndefinedWrapper<
            String
> city;

  AdditionalProperties<Object
?> additionalProperties;

  

  PetsMulticontentTestPostRequestAddress.$all({
        required this.street,
    required this.city,
    required this.additionalProperties,
    
  });

  PetsMulticontentTestPostRequestAddress({
      this.street = const UndefinedWrapper
        .undefined()
,
  this.city = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PetsMulticontentTestPostRequestAddressReflection.instance;
  PetsMulticontentTestPostRequestAddressReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory PetsMulticontentTestPostRequestAddress.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  PetsMulticontentTestPostRequestAddress clone() {
    return $reflection.clone(this);
  }
}








