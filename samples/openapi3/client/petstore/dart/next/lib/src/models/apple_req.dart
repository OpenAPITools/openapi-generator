// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'apple_req.reflection.dart';
part 'apple_req.serialization.dart';


//class defination

///
mixin AppleReqMixin on 
  
  $OpenApiObjectMixin
 {
  String get cultivar;
  UndefinedWrapper<bool> get mealy;


}

///
class AppleReq with
$OpenApiObjectMixin,


AppleReqMixin {
  @override
  String cultivar;
  @override
  UndefinedWrapper<bool> mealy;





  AppleReq.$all({
    required this.cultivar,
    required this.mealy,
    
    
  });

  AppleReq({
  required  this.cultivar ,
    this.mealy = const UndefinedWrapper.undefined(),
    
    
  });
}




