// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'drawing.reflection.dart';
part 'drawing.serialization.dart';


//class defination

///
mixin DrawingMixin on  AdditionalPropertiesMixin<Fruit>,
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<Shape> get mainShape;
  UndefinedWrapper<ShapeOrNull?> get shapeOrNull;
  UndefinedWrapper<NullableShape?> get nullableShape;
  UndefinedWrapper<List<Shape>> get shapes;


}

///
class Drawing with
$OpenApiObjectMixin,
AdditionalPropertiesMixin<Fruit>,

DrawingMixin {
  @override
  UndefinedWrapper<Shape> mainShape;
  @override
  UndefinedWrapper<ShapeOrNull?> shapeOrNull;
  @override
  UndefinedWrapper<NullableShape?> nullableShape;
  @override
  UndefinedWrapper<List<Shape>> shapes;

  @override
  AdditionalProperties<Fruit> additionalProperties;



  Drawing.$all({
    required this.mainShape,
    required this.shapeOrNull,
    required this.nullableShape,
    required this.shapes,
    required this.additionalProperties,
    
  });

  Drawing({
    this.mainShape = const UndefinedWrapper.undefined(),
    this.shapeOrNull = const UndefinedWrapper.undefined(),
    this.nullableShape = const UndefinedWrapper.undefined(),
    this.shapes = const UndefinedWrapper.undefined(),
    this.additionalProperties = const AdditionalProperties(),
    
  });
}




