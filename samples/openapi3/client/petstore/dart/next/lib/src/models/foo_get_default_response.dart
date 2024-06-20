// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'foo_get_default_response.reflection.dart';
part 'foo_get_default_response.serialization.dart';


/// FooGetDefaultResponseMixin
///
/// Properties:
/// * [string] 
mixin FooGetDefaultResponseMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<Foo> get string;

}

/// FooGetDefaultResponse
///
/// Properties:
/// * [string] 
class FooGetDefaultResponse with
$OpenApiObjectMixin,


FooGetDefaultResponseMixin {
  @override
  UndefinedWrapper<Foo> string;





  FooGetDefaultResponse.$all({
    required this.string,
    
    
  });

  FooGetDefaultResponse({
    this.string = const UndefinedWrapper.undefined(),
    
    
  });
}




