// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'basque_pig.reflection.dart';
part 'basque_pig.serialization.dart';


//class defination

///
mixin BasquePigMixin on 
  
  $OpenApiObjectMixin
 {
  String get className;


}

///
class BasquePig with
$OpenApiObjectMixin,


BasquePigMixin {
  @override
  String className;





  BasquePig.$all({
    required this.className,
    
    
  });

  BasquePig({
  required  this.className ,
    
    
  });
}




