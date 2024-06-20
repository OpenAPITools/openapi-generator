// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'scalar.reflection.dart';
part 'scalar.serialization.dart';


//class defination

///
mixin ScalarMixin on 
  
  $OpenApiObjectMixin
 {


UndefinedWrapper<String> get oneOf0;

UndefinedWrapper<num> get oneOf1;

UndefinedWrapper<bool> get oneOf2;

}

///
class Scalar with
$OpenApiObjectMixin,


ScalarMixin {




  @override
UndefinedWrapper<String> oneOf0;

  @override
UndefinedWrapper<num> oneOf1;

  @override
UndefinedWrapper<bool> oneOf2;


  Scalar.$all({
    
    
    required this.oneOf0,
    
    required this.oneOf1,
    
    required this.oneOf2,
    
  });

  Scalar({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    
    this.oneOf1 = const UndefinedWrapper.undefined(),
    
    this.oneOf2 = const UndefinedWrapper.undefined(),
    
  });
}




