// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'apple.reflection.dart';
part 'apple.serialization.dart';


//class defination

///
mixin AppleMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get cultivar;
  UndefinedWrapper<String> get origin;


}

///
class Apple with
$OpenApiObjectMixin,


AppleMixin {
  @override
  UndefinedWrapper<String> cultivar;
  @override
  UndefinedWrapper<String> origin;





  Apple.$all({
    required this.cultivar,
    required this.origin,
    
    
  });

  Apple({
    this.cultivar = const UndefinedWrapper.undefined(),
    this.origin = const UndefinedWrapper.undefined(),
    
    
  });
}




