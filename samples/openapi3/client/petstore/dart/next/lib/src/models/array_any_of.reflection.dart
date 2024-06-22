// Model reflection

part of 'array_any_of.dart';


//class reflection

class ArrayAnyOfReflection extends ClassReflection<ArrayAnyOf> {
  static const instance = ArrayAnyOfReflection._(
  );
  const ArrayAnyOfReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayAnyOf.canDeserialize(src);
  @override
  ArrayAnyOf Function(Object? src) get deserializeFunction =>
      (src) => ArrayAnyOf.deserialize(src);

  @override
  Object? Function(ArrayAnyOf src) get serializeFunction =>
      (src) => src.serialize();
}

class ArrayAnyOfXmlReflection {
    const ArrayAnyOfXmlReflection();
}

