// Model def

import 'package:openapi/_internal.dart';


part 'pets_multicontent_test_post_request_address.reflection.dart';
part 'pets_multicontent_test_post_request_address.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = PetsMulticontentTestPostRequestAddressReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$PetsMulticontentTestPostRequestAddressToMap(this);
  }
  factory PetsMulticontentTestPostRequestAddress.fromMap(Map<String, dynamic> src) {
    return _$PetsMulticontentTestPostRequestAddressFromMap(src);
  }
  static PetsMulticontentTestPostRequestAddress? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return PetsMulticontentTestPostRequestAddress.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$PetsMulticontentTestPostRequestAddressCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory PetsMulticontentTestPostRequestAddress.deserialize(Object? src) {
    return _$PetsMulticontentTestPostRequestAddressDeserialize(src);
  }
  static PetsMulticontentTestPostRequestAddress? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return PetsMulticontentTestPostRequestAddress.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$PetsMulticontentTestPostRequestAddressCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$PetsMulticontentTestPostRequestAddressSerialize(this);
  }
}




