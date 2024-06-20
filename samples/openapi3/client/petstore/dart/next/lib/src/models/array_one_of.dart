// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'array_one_of.reflection.dart';
part 'array_one_of.serialization.dart';


//class defination

///
mixin ArrayOneOfMixin on 
  
  $OpenApiObjectMixin
 {


UndefinedWrapper<int> get oneOf0;

UndefinedWrapper<List<String>> get oneOf1;

}

///
class ArrayOneOf with
$OpenApiObjectMixin,


ArrayOneOfMixin {




  @override
UndefinedWrapper<int> oneOf0;

  @override
UndefinedWrapper<List<String>> oneOf1;


  ArrayOneOf.$all({
    
    
    required this.oneOf0,
    
    required this.oneOf1,
    
  });

  ArrayOneOf({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    
    this.oneOf1 = const UndefinedWrapper.undefined(),
    
  });
}




