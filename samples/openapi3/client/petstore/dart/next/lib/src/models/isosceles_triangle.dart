// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'isosceles_triangle.reflection.dart';
part 'isosceles_triangle.serialization.dart';

//class defination

///
mixin IsoscelesTriangleMixin
    on ShapeInterfaceMixin, TriangleInterfaceMixin, $OpenApiObjectMixin {}

///
class IsoscelesTriangle
    with
        $OpenApiObjectMixin,
        ShapeInterfaceMixin,
        TriangleInterfaceMixin,
        IsoscelesTriangleMixin {
  @override
  String shapeType;
  @override
  String triangleType;

  IsoscelesTriangle.$all({
    required this.shapeType,
    required this.triangleType,
  });

  IsoscelesTriangle({
    required this.shapeType,
    required this.triangleType,
  });
}
