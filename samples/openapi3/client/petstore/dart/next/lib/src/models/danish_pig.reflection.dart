// Model reflection

part of 'danish_pig.dart';


//class reflection

class DanishPigReflection extends ClassReflection<DanishPig> {
  static const instance = DanishPigReflection._(
    className: PropertyReflection(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
    ),
  );
  const DanishPigReflection._({
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
    (src) => DanishPig.canDeserialize(src);
  @override
  DanishPig Function(Object? src) get deserializeFunction =>
      (src) => DanishPig.deserialize(src);

  @override
  Object? Function(DanishPig src) get serializeFunction =>
      (src) => src.serialize();
}

class DanishPigXmlReflection {
    const DanishPigXmlReflection();
}

