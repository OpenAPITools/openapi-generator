// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'update_pet_with_form_request.reflection.dart';
part 'update_pet_with_form_request.serialization.dart';


/// UpdatePetWithFormRequestMixin
///
/// Properties:
/// * [name] - Updated name of the pet
/// * [status] - Updated status of the pet
mixin UpdatePetWithFormRequestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get name;
  UndefinedWrapper<String> get status;

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
  UndefinedWrapper<String> name;
  @override
  UndefinedWrapper<String> status;





  UpdatePetWithFormRequest.$all({
    required this.name,
    required this.status,
    
    
  });

  UpdatePetWithFormRequest({
    this.name = const UndefinedWrapper.undefined(),
    this.status = const UndefinedWrapper.undefined(),
    
    
  });
}




