// Model reflection

part of 'pig.dart';


//class reflection

class PigReflection extends ClassReflection<Pig> {
  static const instance = PigReflection._(
  );
  const PigReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Pig.canDeserialize(src);
  @override
  Pig Function(Object? src) get deserializeFunction =>
      (src) => Pig.deserialize(src);

  @override
  Object? Function(Pig src) get serializeFunction =>
      (src) => src.serialize();
}

class PigXmlReflection {
    const PigXmlReflection();
}

