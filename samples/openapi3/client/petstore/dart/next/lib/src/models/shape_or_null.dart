// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'shape_or_null.reflection.dart';
part 'shape_or_null.serialization.dart';

//class defination

///
mixin ShapeOrNullMixin on $OpenApiObjectMixin {
  UndefinedWrapper<Triangle> get oneOf0;

  UndefinedWrapper<Quadrilateral> get oneOf1;
}

///
class ShapeOrNull with $OpenApiObjectMixin, ShapeOrNullMixin {
  @override
  UndefinedWrapper<Triangle> oneOf0;

  @override
  UndefinedWrapper<Quadrilateral> oneOf1;

  ShapeOrNull.$all({
    required this.oneOf0,
    required this.oneOf1,
  });

  ShapeOrNull({
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}
