// Model reflection

part of 'triangle.dart';


//class reflection

class TriangleReflection extends ClassReflection<Triangle> {
  static const instance = TriangleReflection._(
  );
  const TriangleReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Triangle.canDeserialize(src);
  @override
  Triangle Function(Object? src) get deserializeFunction =>
      (src) => Triangle.deserialize(src);

  @override
  Object? Function(Triangle src) get serializeFunction =>
      (src) => src.serialize();
}

class TriangleXmlReflection {
    const TriangleXmlReflection();
}

