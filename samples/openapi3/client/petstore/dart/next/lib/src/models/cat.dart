// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'cat.reflection.dart';
part 'cat.serialization.dart';


//class defination

///
mixin CatMixin on 
  AnimalMixin,
  $OpenApiObjectMixin
 {
  UndefinedWrapper<bool> get declawed;


}

///
class Cat with
$OpenApiObjectMixin,

AnimalMixin,
CatMixin {
  @override
  UndefinedWrapper<String> color;
  @override
  UndefinedWrapper<bool> declawed;
  @override
  String className;





  Cat.$all({
    required this.color,
    required this.declawed,
    required this.className,
    
    
  });

  Cat({
    this.color = const UndefinedWrapper('red'),
    this.declawed = const UndefinedWrapper.undefined(),
  required  this.className ,
    
    
  });
}




