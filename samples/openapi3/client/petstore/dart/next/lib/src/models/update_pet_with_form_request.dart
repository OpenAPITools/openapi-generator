// Model def

import 'package:petstore_api/_internal.dart';


part 'update_pet_with_form_request.reflection.dart';


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

  AdditionalProperties<Object
?> additionalProperties;

  

  UpdatePetWithFormRequest.$all({
        required this.name,
    required this.status,
    required this.additionalProperties,
    
  });

  UpdatePetWithFormRequest({
      this.name = const UndefinedWrapper
        .undefined()
,
  this.status = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = UpdatePetWithFormRequestReflection.instance;
  UpdatePetWithFormRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory UpdatePetWithFormRequest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  UpdatePetWithFormRequest clone() {
    return $reflection.clone(this);
  }
}








