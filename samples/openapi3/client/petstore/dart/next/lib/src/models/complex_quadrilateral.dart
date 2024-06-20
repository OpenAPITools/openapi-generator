// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'complex_quadrilateral.reflection.dart';
part 'complex_quadrilateral.serialization.dart';


/// ComplexQuadrilateralMixin
///
/// Properties:
mixin ComplexQuadrilateralMixin on 
  QuadrilateralInterfaceMixin,ShapeInterfaceMixin,
  $OpenApiObjectMixin
 {

}

/// ComplexQuadrilateral
///
/// Properties:
/// * [quadrilateralType] 
/// * [shapeType] 
class ComplexQuadrilateral with
$OpenApiObjectMixin,

QuadrilateralInterfaceMixin,ShapeInterfaceMixin,
ComplexQuadrilateralMixin {
  @override
  String quadrilateralType;
  @override
  String shapeType;





  ComplexQuadrilateral.$all({
    required this.quadrilateralType,
    required this.shapeType,
    
    
  });

  ComplexQuadrilateral({
  required  this.quadrilateralType ,
  required  this.shapeType ,
    
    
  });
}




