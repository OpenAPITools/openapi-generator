// Model reflection

part of 'value.dart';


//class reflection

class ValueReflection extends ClassReflection<Value> {
  static const instance = ValueReflection._(
  );
  const ValueReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Value.canDeserialize(src);
  @override
  Value Function(Object? src) get deserializeFunction =>
      (src) => Value.deserialize(src);

  @override
  Object? Function(Value src) get serializeFunction =>
      (src) => src.serialize();
}

class ValueXmlReflection {
    const ValueXmlReflection();
}

