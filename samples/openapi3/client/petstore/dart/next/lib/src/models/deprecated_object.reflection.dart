// Model reflection

part of 'deprecated_object.dart';


//class reflection

class DeprecatedObjectReflection extends ClassReflection<DeprecatedObject> {
  static const instance = DeprecatedObjectReflection._(
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
  );
  const DeprecatedObjectReflection._({
    required this.name,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> name;

  @override
  List<PropertyReflection> get members => [
    name,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => DeprecatedObject.canDeserialize(src);
  @override
  DeprecatedObject Function(Object? src) get deserializeFunction =>
      (src) => DeprecatedObject.deserialize(src);

  @override
  Object? Function(DeprecatedObject src) get serializeFunction =>
      (src) => src.serialize();
}

class DeprecatedObjectXmlReflection {
    const DeprecatedObjectXmlReflection();
}

