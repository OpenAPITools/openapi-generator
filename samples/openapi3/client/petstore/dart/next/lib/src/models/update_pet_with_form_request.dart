// Model def

import 'package:openapi/_internal.dart';


part 'update_pet_with_form_request.reflection.dart';
part 'update_pet_with_form_request.serialization.dart';


/// UpdatePetWithFormRequestMixin
///
/// Properties:
/// * [name] - Updated name of the pet
/// * [status] - Updated status of the pet
mixin UpdatePetWithFormRequestMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get name;
UndefinedWrapper<
            String
> get status;
  
}

/// UpdatePetWithFormRequest
///
/// Properties:
/// * [name] - Updated name of the pet
/// * [status] - Updated status of the pet
class UpdatePetWithFormRequest with
$OpenApiObjectMixin,


UpdatePetWithFormRequestMixin {
  @override
  UndefinedWrapper<
            String
> name;
  @override
  UndefinedWrapper<
            String
> status;

  

  

  UpdatePetWithFormRequest.$all({
        required this.name,
    required this.status,
    
    
  });

  UpdatePetWithFormRequest({
      this.name = const UndefinedWrapper
        .undefined()
,
  this.status = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = UpdatePetWithFormRequestReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$UpdatePetWithFormRequestToMap(this);
  }
  factory UpdatePetWithFormRequest.fromMap(Map<String, dynamic> src) {
    return _$UpdatePetWithFormRequestFromMap(src);
  }
  static UpdatePetWithFormRequest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return UpdatePetWithFormRequest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$UpdatePetWithFormRequestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory UpdatePetWithFormRequest.deserialize(Object? src) {
    return _$UpdatePetWithFormRequestDeserialize(src);
  }
  static UpdatePetWithFormRequest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return UpdatePetWithFormRequest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$UpdatePetWithFormRequestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$UpdatePetWithFormRequestSerialize(this);
  }
}




