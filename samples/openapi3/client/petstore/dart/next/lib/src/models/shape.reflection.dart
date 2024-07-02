// Model reflection

part of 'shape.dart';


//class reflection

class ShapeReflection extends ClassReflection<Shape> {
  static const instance = ShapeReflection._(
  );
  const ShapeReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Shape.canDeserialize(src);
  @override
  Shape Function(Object? src) get deserializeFunction =>
      (src) => Shape.deserialize(src);

  @override
  Object? Function(Shape src) get serializeFunction =>
      (src) => src.serialize();
}

class ShapeXmlReflection {
    const ShapeXmlReflection();
}

