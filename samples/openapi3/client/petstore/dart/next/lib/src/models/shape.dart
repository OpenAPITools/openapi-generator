// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'shape.reflection.dart';
part 'shape.serialization.dart';


/// ShapeMixin
mixin ShapeMixin on 
  
  $OpenApiObjectMixin
 {

  UndefinedWrapper<Triangle> get oneOf0;
  UndefinedWrapper<Quadrilateral> get oneOf1;
}

/// Shape
class Shape with
$OpenApiObjectMixin,


ShapeMixin {




  @override
  UndefinedWrapper<Triangle> oneOf0;

  @override
  UndefinedWrapper<Quadrilateral> oneOf1;


  Shape.$all({
    
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Shape({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}




