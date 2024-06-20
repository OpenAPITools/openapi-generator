// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'deprecated_object.reflection.dart';
part 'deprecated_object.serialization.dart';


//class defination

///
mixin DeprecatedObjectMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get name;


}

///
class DeprecatedObject with
$OpenApiObjectMixin,


DeprecatedObjectMixin {
  @override
  UndefinedWrapper<String> name;





  DeprecatedObject.$all({
    required this.name,
    
    
  });

  DeprecatedObject({
    this.name = const UndefinedWrapper.undefined(),
    
    
  });
}




