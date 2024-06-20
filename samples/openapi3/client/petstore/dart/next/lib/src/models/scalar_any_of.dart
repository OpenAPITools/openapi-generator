// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'scalar_any_of.reflection.dart';
part 'scalar_any_of.serialization.dart';


//class defination

///
mixin ScalarAnyOfMixin on 
  
  $OpenApiObjectMixin
 {


UndefinedWrapper<String> get anyOf0;

UndefinedWrapper<num> get anyOf1;

UndefinedWrapper<bool> get anyOf2;

}

///
class ScalarAnyOf with
$OpenApiObjectMixin,


ScalarAnyOfMixin {




  @override
UndefinedWrapper<String> anyOf0;

  @override
UndefinedWrapper<num> anyOf1;

  @override
UndefinedWrapper<bool> anyOf2;


  ScalarAnyOf.$all({
    
    
    required this.anyOf0,
    
    required this.anyOf1,
    
    required this.anyOf2,
    
  });

  ScalarAnyOf({
    
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
    this.anyOf2 = const UndefinedWrapper.undefined(),
    
  });
}




