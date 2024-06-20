// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pig.reflection.dart';
part 'pig.serialization.dart';


//class defination

///
mixin PigMixin on 
  
  $OpenApiObjectMixin
 {


UndefinedWrapper<BasquePig> get oneOf0;

UndefinedWrapper<DanishPig> get oneOf1;

}

///
class Pig with
$OpenApiObjectMixin,


PigMixin {




  @override
UndefinedWrapper<BasquePig> oneOf0;

  @override
UndefinedWrapper<DanishPig> oneOf1;


  Pig.$all({
    
    
    required this.oneOf0,
    
    required this.oneOf1,
    
  });

  Pig({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    
    this.oneOf1 = const UndefinedWrapper.undefined(),
    
  });
}




