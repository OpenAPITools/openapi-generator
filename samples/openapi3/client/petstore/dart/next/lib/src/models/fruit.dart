// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'fruit.reflection.dart';
part 'fruit.serialization.dart';


/// FruitMixin
///
/// Properties:
/// * [color] 
mixin FruitMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get color;

  UndefinedWrapper<Apple?> get oneOf0;
  UndefinedWrapper<Banana> get oneOf1;
}

/// Fruit
///
/// Properties:
/// * [color] 
class Fruit with
$OpenApiObjectMixin,


FruitMixin {
  @override
  UndefinedWrapper<String> color;




  @override
  UndefinedWrapper<Apple?> oneOf0;

  @override
  UndefinedWrapper<Banana> oneOf1;


  Fruit.$all({
    required this.color,
    
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Fruit({
    this.color = const UndefinedWrapper.undefined(),
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}




