// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pets_multicontent_test_post_request_address.reflection.dart';
part 'pets_multicontent_test_post_request_address.serialization.dart';


/// PetsMulticontentTestPostRequestAddressMixin
///
/// Properties:
/// * [street] 
/// * [city] 
mixin PetsMulticontentTestPostRequestAddressMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get street;
  UndefinedWrapper<String> get city;

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
  UndefinedWrapper<String> street;
  @override
  UndefinedWrapper<String> city;





  PetsMulticontentTestPostRequestAddress.$all({
    required this.street,
    required this.city,
    
    
  });

  PetsMulticontentTestPostRequestAddress({
    this.street = const UndefinedWrapper.undefined(),
    this.city = const UndefinedWrapper.undefined(),
    
    
  });
}




