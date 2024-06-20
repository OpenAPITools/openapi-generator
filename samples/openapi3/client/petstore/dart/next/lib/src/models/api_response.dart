// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'api_response.reflection.dart';
part 'api_response.serialization.dart';


/// ApiResponseMixin
///
/// Properties:
/// * [code] 
/// * [type] 
/// * [message] 
mixin ApiResponseMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int> get code;
  UndefinedWrapper<String> get type;
  UndefinedWrapper<String> get message;

}

/// ApiResponse
///
/// Properties:
/// * [code] 
/// * [type] 
/// * [message] 
class ApiResponse with
$OpenApiObjectMixin,


ApiResponseMixin {
  @override
  UndefinedWrapper<int> code;
  @override
  UndefinedWrapper<String> type;
  @override
  UndefinedWrapper<String> message;





  ApiResponse.$all({
    required this.code,
    required this.type,
    required this.message,
    
    
  });

  ApiResponse({
    this.code = const UndefinedWrapper.undefined(),
    this.type = const UndefinedWrapper.undefined(),
    this.message = const UndefinedWrapper.undefined(),
    
    
  });
}




