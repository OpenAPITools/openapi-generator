// Model reflection

part of 'scalar.dart';


//class reflection

class ScalarReflection extends ClassReflection<Scalar> {
  static const instance = ScalarReflection._(
  );
  const ScalarReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Scalar.canDeserialize(src);
  @override
  Scalar Function(Object? src) get deserializeFunction =>
      (src) => Scalar.deserialize(src);

  @override
  Object? Function(Scalar src) get serializeFunction =>
      (src) => src.serialize();
}

class ScalarXmlReflection {
    const ScalarXmlReflection();
}

