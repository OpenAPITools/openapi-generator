// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pets_multicontent_test_post_request.reflection.dart';
part 'pets_multicontent_test_post_request.serialization.dart';


/// PetsMulticontentTestPostRequestMixin
///
/// Properties:
/// * [id] 
/// * [address] 
/// * [profileImage] 
mixin PetsMulticontentTestPostRequestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get id;
  UndefinedWrapper<PetsMulticontentTestPostRequestAddress> get address;
  UndefinedWrapper<MyFile> get profileImage;

}

/// PetsMulticontentTestPostRequest
///
/// Properties:
/// * [id] 
/// * [address] 
/// * [profileImage] 
class PetsMulticontentTestPostRequest with
$OpenApiObjectMixin,


PetsMulticontentTestPostRequestMixin {
  @override
  UndefinedWrapper<String> id;
  @override
  UndefinedWrapper<PetsMulticontentTestPostRequestAddress> address;
  @override
  UndefinedWrapper<MyFile> profileImage;





  PetsMulticontentTestPostRequest.$all({
    required this.id,
    required this.address,
    required this.profileImage,
    
    
  });

  PetsMulticontentTestPostRequest({
    this.id = const UndefinedWrapper.undefined(),
    this.address = const UndefinedWrapper.undefined(),
    this.profileImage = const UndefinedWrapper.undefined(),
    
    
  });
}




