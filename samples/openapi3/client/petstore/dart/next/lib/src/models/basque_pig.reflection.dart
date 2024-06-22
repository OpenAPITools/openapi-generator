// Model reflection

part of 'basque_pig.dart';


//class reflection

class BasquePigReflection extends ClassReflection<BasquePig> {
  static const instance = BasquePigReflection._(
    className: PropertyReflection(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
    ),
  );
  const BasquePigReflection._({
    required this.className,
  });

  final PropertyReflection<
            String
> className;

  @override
  List<PropertyReflection> get members => [
    className,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => BasquePig.canDeserialize(src);
  @override
  BasquePig Function(Object? src) get deserializeFunction =>
      (src) => BasquePig.deserialize(src);

  @override
  Object? Function(BasquePig src) get serializeFunction =>
      (src) => src.serialize();
}

class BasquePigXmlReflection {
    const BasquePigXmlReflection();
}

