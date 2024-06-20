// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'scalene_triangle.reflection.dart';
part 'scalene_triangle.serialization.dart';


/// ScaleneTriangleMixin
///
/// Properties:
mixin ScaleneTriangleMixin on 
  ShapeInterfaceMixin,TriangleInterfaceMixin,
  $OpenApiObjectMixin
 {

}

/// ScaleneTriangle
///
/// Properties:
/// * [shapeType] 
/// * [triangleType] 
class ScaleneTriangle with
$OpenApiObjectMixin,

ShapeInterfaceMixin,TriangleInterfaceMixin,
ScaleneTriangleMixin {
  @override
  String shapeType;
  @override
  String triangleType;





  ScaleneTriangle.$all({
    required this.shapeType,
    required this.triangleType,
    
    
  });

  ScaleneTriangle({
  required  this.shapeType ,
  required  this.triangleType ,
    
    
  });
}




