// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'test_json_form_data_request.reflection.dart';
part 'test_json_form_data_request.serialization.dart';


/// TestJsonFormDataRequestMixin
///
/// Properties:
/// * [param] - field1
/// * [param2] - field2
mixin TestJsonFormDataRequestMixin on 
  
  $OpenApiObjectMixin
 {
  String get param;
  String get param2;

}

/// TestJsonFormDataRequest
///
/// Properties:
/// * [param] - field1
/// * [param2] - field2
class TestJsonFormDataRequest with
$OpenApiObjectMixin,


TestJsonFormDataRequestMixin {
  @override
  String param;
  @override
  String param2;





  TestJsonFormDataRequest.$all({
    required this.param,
    required this.param2,
    
    
  });

  TestJsonFormDataRequest({
  required  this.param ,
  required  this.param2 ,
    
    
  });
}




