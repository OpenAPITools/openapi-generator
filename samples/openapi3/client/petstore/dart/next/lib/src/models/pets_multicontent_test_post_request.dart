// Model def

import 'package:openapi/_internal.dart';


part 'pets_multicontent_test_post_request.reflection.dart';
part 'pets_multicontent_test_post_request.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = PetsMulticontentTestPostRequestReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$PetsMulticontentTestPostRequestToMap(this);
  }
  factory PetsMulticontentTestPostRequest.fromMap(Map<String, dynamic> src) {
    return _$PetsMulticontentTestPostRequestFromMap(src);
  }
  static PetsMulticontentTestPostRequest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return PetsMulticontentTestPostRequest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PetsMulticontentTestPostRequestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory PetsMulticontentTestPostRequest.deserialize(Object? src) {
    return _$PetsMulticontentTestPostRequestDeserialize(src);
  }
  static PetsMulticontentTestPostRequest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return PetsMulticontentTestPostRequest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PetsMulticontentTestPostRequestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$PetsMulticontentTestPostRequestSerialize(this);
  }
}




