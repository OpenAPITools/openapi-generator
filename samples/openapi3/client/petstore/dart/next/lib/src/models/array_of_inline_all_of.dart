// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'array_of_inline_all_of.reflection.dart';
part 'array_of_inline_all_of.serialization.dart';


//class defination

///
mixin ArrayOfInlineAllOfMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int> get id;
  String get name;
  UndefinedWrapper<List<ArrayOfInlineAllOfArrayAllofDogPropertyInner>> get arrayAllofDogProperty;


}

///
class ArrayOfInlineAllOf with
$OpenApiObjectMixin,


ArrayOfInlineAllOfMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  String name;
  @override
  UndefinedWrapper<List<ArrayOfInlineAllOfArrayAllofDogPropertyInner>> arrayAllofDogProperty;





  ArrayOfInlineAllOf.$all({
    required this.id,
    required this.name,
    required this.arrayAllofDogProperty,
    
    
  });

  ArrayOfInlineAllOf({
    this.id = const UndefinedWrapper.undefined(),
  required  this.name ,
    this.arrayAllofDogProperty = const UndefinedWrapper.undefined(),
    
    
  });
}




