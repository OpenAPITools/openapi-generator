// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'nullable_shape.reflection.dart';
part 'nullable_shape.serialization.dart';


/// The value may be a shape or the 'null' value. The 'nullable' attribute was introduced in OAS schema >= 3.0 and has been deprecated in OAS schema >= 3.1.
mixin NullableShapeMixin on 
  
  $OpenApiObjectMixin
 {

  UndefinedWrapper<Triangle> get oneOf0;
  UndefinedWrapper<Quadrilateral> get oneOf1;
}

/// The value may be a shape or the 'null' value. The 'nullable' attribute was introduced in OAS schema >= 3.0 and has been deprecated in OAS schema >= 3.1.
class NullableShape with
$OpenApiObjectMixin,


NullableShapeMixin {




  @override
  UndefinedWrapper<Triangle> oneOf0;

  @override
  UndefinedWrapper<Quadrilateral> oneOf1;


  NullableShape.$all({
    
    
    required this.oneOf0,
    required this.oneOf1,
  });

  NullableShape({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}




