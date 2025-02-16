// Model def

import 'package:petstore_api/_internal.dart';


part 'pets_multicontent_test_post_request.reflection.dart';


/// PetsMulticontentTestPostRequestMixin
///
/// Properties:
/// * [id] 
/// * [address] 
/// * [profileImages] 
mixin PetsMulticontentTestPostRequestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get id;
UndefinedWrapper<
            PetsMulticontentTestPostRequestAddress
> get address;
UndefinedWrapper<
    List<
        
            XFile
>
> get profileImages;
  
}

/// PetsMulticontentTestPostRequest
///
/// Properties:
/// * [id] 
/// * [address] 
/// * [profileImages] 
class PetsMulticontentTestPostRequest with
$OpenApiObjectMixin,

PetsMulticontentTestPostRequestMixin {
  @override
  UndefinedWrapper<
            String
> id;
  @override
  UndefinedWrapper<
            PetsMulticontentTestPostRequestAddress
> address;
  @override
  UndefinedWrapper<
    List<
        
            XFile
>
> profileImages;

  AdditionalProperties<Object
?> additionalProperties;

  

  PetsMulticontentTestPostRequest.$all({
        required this.id,
    required this.address,
    required this.profileImages,
    required this.additionalProperties,
    
  });

  PetsMulticontentTestPostRequest({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.address = const UndefinedWrapper
        .undefined()
,
  this.profileImages = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = PetsMulticontentTestPostRequestReflection.instance;
  PetsMulticontentTestPostRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory PetsMulticontentTestPostRequest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  PetsMulticontentTestPostRequest clone() {
    return $reflection.clone(this);
  }
}













