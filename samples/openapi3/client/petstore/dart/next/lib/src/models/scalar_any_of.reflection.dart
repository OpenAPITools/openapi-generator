// Model reflection

part of 'scalar_any_of.dart';


//class reflection

class ScalarAnyOfReflection extends ClassReflection<ScalarAnyOf> {
  static const instance = ScalarAnyOfReflection._(
  );
  const ScalarAnyOfReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ScalarAnyOf.canDeserialize(src);
  @override
  ScalarAnyOf Function(Object? src) get deserializeFunction =>
      (src) => ScalarAnyOf.deserialize(src);

  @override
  Object? Function(ScalarAnyOf src) get serializeFunction =>
      (src) => src.serialize();
}

class ScalarAnyOfXmlReflection {
    const ScalarAnyOfXmlReflection();
}

