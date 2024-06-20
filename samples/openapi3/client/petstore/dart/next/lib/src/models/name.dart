// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'name.reflection.dart';
part 'name.serialization.dart';


/// Model for testing model name same as property name
///
/// Properties:
/// * [name] 
/// * [snakeCase] 
/// * [property] 
/// * [$123number] 
mixin NameMixin on 
  
  $OpenApiObjectMixin
 {
  int get name;
  UndefinedWrapper<int> get snakeCase;
  UndefinedWrapper<String> get property;
  UndefinedWrapper<int> get $123number;

}

/// Model for testing model name same as property name
///
/// Properties:
/// * [name] 
/// * [snakeCase] 
/// * [property] 
/// * [$123number] 
class Name with
$OpenApiObjectMixin,


NameMixin {
  @override
  int name;
  @override
  UndefinedWrapper<int> snakeCase;
  @override
  UndefinedWrapper<String> property;
  @override
  UndefinedWrapper<int> $123number;





  Name.$all({
    required this.name,
    required this.snakeCase,
    required this.property,
    required this.$123number,
    
    
  });

  Name({
  required  this.name ,
    this.snakeCase = const UndefinedWrapper.undefined(),
    this.property = const UndefinedWrapper.undefined(),
    this.$123number = const UndefinedWrapper.undefined(),
    
    
  });
}




