// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'triangle_interface.reflection.dart';
part 'triangle_interface.serialization.dart';


/// TriangleInterfaceMixin
///
/// Properties:
/// * [triangleType] 
mixin TriangleInterfaceMixin on 
  
  $OpenApiObjectMixin
 {
  String get triangleType;

}

/// TriangleInterface
///
/// Properties:
/// * [triangleType] 
class TriangleInterface with
$OpenApiObjectMixin,


TriangleInterfaceMixin {
  @override
  String triangleType;





  TriangleInterface.$all({
    required this.triangleType,
    
    
  });

  TriangleInterface({
  required  this.triangleType ,
    
    
  });
}




