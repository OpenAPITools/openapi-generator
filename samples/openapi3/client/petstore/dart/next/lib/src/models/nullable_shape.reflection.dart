// Model reflection

part of 'nullable_shape.dart';


//class reflection

class NullableShapeReflection extends ClassReflection<NullableShape> {
  static const instance = NullableShapeReflection._(
  );
  const NullableShapeReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NullableShape.canDeserialize(src);
  @override
  NullableShape Function(Object? src) get deserializeFunction =>
      (src) => NullableShape.deserialize(src);

  @override
  Object? Function(NullableShape src) get serializeFunction =>
      (src) => src.serialize();
}

class NullableShapeXmlReflection {
    const NullableShapeXmlReflection();
}

