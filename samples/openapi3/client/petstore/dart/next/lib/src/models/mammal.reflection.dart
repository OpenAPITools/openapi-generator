// Model reflection

part of 'mammal.dart';


//class reflection

class MammalReflection extends ClassReflection<Mammal> {
  static const instance = MammalReflection._(
  );
  const MammalReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Mammal.canDeserialize(src);
  @override
  Mammal Function(Object? src) get deserializeFunction =>
      (src) => Mammal.deserialize(src);

  @override
  Object? Function(Mammal src) get serializeFunction =>
      (src) => src.serialize();
}

class MammalXmlReflection {
    const MammalXmlReflection();
}

