// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'fake_one_of_w_ith_same_erasure_get200_response.reflection.dart';
part 'fake_one_of_w_ith_same_erasure_get200_response.serialization.dart';


/// FakeOneOfWIthSameErasureGet200ResponseMixin
mixin FakeOneOfWIthSameErasureGet200ResponseMixin on 
  
  $OpenApiObjectMixin
 {

  UndefinedWrapper<List<String>> get oneOf0;
  UndefinedWrapper<List<int>> get oneOf1;
}

/// FakeOneOfWIthSameErasureGet200Response
class FakeOneOfWIthSameErasureGet200Response with
$OpenApiObjectMixin,


FakeOneOfWIthSameErasureGet200ResponseMixin {




  @override
  UndefinedWrapper<List<String>> oneOf0;

  @override
  UndefinedWrapper<List<int>> oneOf1;


  FakeOneOfWIthSameErasureGet200Response.$all({
    
    
    required this.oneOf0,
    required this.oneOf1,
  });

  FakeOneOfWIthSameErasureGet200Response({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}




