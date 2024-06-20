// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'banana.reflection.dart';
part 'banana.serialization.dart';


//class defination

///
mixin BananaMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<num> get lengthCm;


}

///
class Banana with
$OpenApiObjectMixin,


BananaMixin {
  @override
  UndefinedWrapper<num> lengthCm;





  Banana.$all({
    required this.lengthCm,
    
    
  });

  Banana({
    this.lengthCm = const UndefinedWrapper.undefined(),
    
    
  });
}




