// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'equilateral_triangle.reflection.dart';
part 'equilateral_triangle.serialization.dart';

//class defination

///
mixin EquilateralTriangleMixin
    on ShapeInterfaceMixin, TriangleInterfaceMixin, $OpenApiObjectMixin {}

///
class EquilateralTriangle
    with
        $OpenApiObjectMixin,
        ShapeInterfaceMixin,
        TriangleInterfaceMixin,
        EquilateralTriangleMixin {
  @override
  String shapeType;
  @override
  String triangleType;

  EquilateralTriangle.$all({
    required this.shapeType,
    required this.triangleType,
  });

  EquilateralTriangle({
    required this.shapeType,
    required this.triangleType,
  });
}
