// Model reflection

part of 'quadrilateral.dart';


//class reflection

class QuadrilateralReflection extends ClassReflection<Quadrilateral> {
  static const instance = QuadrilateralReflection._(
  );
  const QuadrilateralReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Quadrilateral.canDeserialize(src);
  @override
  Quadrilateral Function(Object? src) get deserializeFunction =>
      (src) => Quadrilateral.deserialize(src);

  @override
  Object? Function(Quadrilateral src) get serializeFunction =>
      (src) => src.serialize();
}

class QuadrilateralXmlReflection {
    const QuadrilateralXmlReflection();
}

