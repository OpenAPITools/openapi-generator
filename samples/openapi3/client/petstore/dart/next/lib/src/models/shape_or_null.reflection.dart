// Model reflection

part of 'shape_or_null.dart';


//class reflection

class ShapeOrNullReflection extends ClassReflection<ShapeOrNull> {
  static const instance = ShapeOrNullReflection._(
  );
  const ShapeOrNullReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ShapeOrNull.canDeserialize(src);
  @override
  ShapeOrNull Function(Object? src) get deserializeFunction =>
      (src) => ShapeOrNull.deserialize(src);

  @override
  Object? Function(ShapeOrNull src) get serializeFunction =>
      (src) => src.serialize();
}

class ShapeOrNullXmlReflection {
    const ShapeOrNullXmlReflection();
}

