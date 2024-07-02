// Model reflection

part of 'array_one_of.dart';


//class reflection

class ArrayOneOfReflection extends ClassReflection<ArrayOneOf> {
  static const instance = ArrayOneOfReflection._(
  );
  const ArrayOneOfReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOneOf.canDeserialize(src);
  @override
  ArrayOneOf Function(Object? src) get deserializeFunction =>
      (src) => ArrayOneOf.deserialize(src);

  @override
  Object? Function(ArrayOneOf src) get serializeFunction =>
      (src) => src.serialize();
}

class ArrayOneOfXmlReflection {
    const ArrayOneOfXmlReflection();
}

