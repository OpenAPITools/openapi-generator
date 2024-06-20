// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'quadrilateral.reflection.dart';
part 'quadrilateral.serialization.dart';

//class defination

///
mixin QuadrilateralMixin on $OpenApiObjectMixin {
  UndefinedWrapper<SimpleQuadrilateral> get oneOf0;

  UndefinedWrapper<ComplexQuadrilateral> get oneOf1;
}

///
class Quadrilateral with $OpenApiObjectMixin, QuadrilateralMixin {
  @override
  UndefinedWrapper<SimpleQuadrilateral> oneOf0;

  @override
  UndefinedWrapper<ComplexQuadrilateral> oneOf1;

  Quadrilateral.$all({
    required this.oneOf0,
    required this.oneOf1,
  });

  Quadrilateral({
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}
