// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'triangle.reflection.dart';
part 'triangle.serialization.dart';

//class defination

///
mixin TriangleMixin on $OpenApiObjectMixin {
  UndefinedWrapper<EquilateralTriangle> get oneOf0;

  UndefinedWrapper<IsoscelesTriangle> get oneOf1;

  UndefinedWrapper<ScaleneTriangle> get oneOf2;
}

///
class Triangle with $OpenApiObjectMixin, TriangleMixin {
  @override
  UndefinedWrapper<EquilateralTriangle> oneOf0;

  @override
  UndefinedWrapper<IsoscelesTriangle> oneOf1;

  @override
  UndefinedWrapper<ScaleneTriangle> oneOf2;

  Triangle.$all({
    required this.oneOf0,
    required this.oneOf1,
    required this.oneOf2,
  });

  Triangle({
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
    this.oneOf2 = const UndefinedWrapper.undefined(),
  });
}
