// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'quadrilateral_interface.reflection.dart';
part 'quadrilateral_interface.serialization.dart';


/// QuadrilateralInterfaceMixin
///
/// Properties:
/// * [quadrilateralType] 
mixin QuadrilateralInterfaceMixin on 
  
  $OpenApiObjectMixin
 {
  String get quadrilateralType;

}

/// QuadrilateralInterface
///
/// Properties:
/// * [quadrilateralType] 
class QuadrilateralInterface with
$OpenApiObjectMixin,


QuadrilateralInterfaceMixin {
  @override
  String quadrilateralType;





  QuadrilateralInterface.$all({
    required this.quadrilateralType,
    
    
  });

  QuadrilateralInterface({
  required  this.quadrilateralType ,
    
    
  });
}




