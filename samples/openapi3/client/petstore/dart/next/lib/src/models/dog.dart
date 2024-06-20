// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'dog.reflection.dart';
part 'dog.serialization.dart';


/// DogMixin
///
/// Properties:
/// * [breed] 
mixin DogMixin on 
  AnimalMixin,
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get breed;

}

/// Dog
///
/// Properties:
/// * [color] 
/// * [breed] 
/// * [className] 
class Dog with
$OpenApiObjectMixin,

AnimalMixin,
DogMixin {
  @override
  UndefinedWrapper<String> color;
  @override
  UndefinedWrapper<String> breed;
  @override
  String className;





  Dog.$all({
    required this.color,
    required this.breed,
    required this.className,
    
    
  });

  Dog({
    this.color = const UndefinedWrapper('red'),
    this.breed = const UndefinedWrapper.undefined(),
  required  this.className ,
    
    
  });
}




