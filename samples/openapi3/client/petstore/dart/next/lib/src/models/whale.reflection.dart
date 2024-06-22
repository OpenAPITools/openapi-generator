// Model reflection

part of 'whale.dart';


//class reflection

class WhaleReflection extends ClassReflection<Whale> {
  static const instance = WhaleReflection._(
    hasBaleen: PropertyReflection(
      dartName: r'hasBaleen',
      nullable: false,
      required: false,
      oasName: r'hasBaleen',
      oasType: r'boolean',
      pattern: null,
    ),
    hasTeeth: PropertyReflection(
      dartName: r'hasTeeth',
      nullable: false,
      required: false,
      oasName: r'hasTeeth',
      oasType: r'boolean',
      pattern: null,
    ),
    className: PropertyReflection(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
    ),
  );
  const WhaleReflection._({
    required this.hasBaleen,
  
    required this.hasTeeth,
  
    required this.className,
  });

  final PropertyReflection<UndefinedWrapper<
            bool
>> hasBaleen;
  final PropertyReflection<UndefinedWrapper<
            bool
>> hasTeeth;
  final PropertyReflection<
            String
> className;

  @override
  List<PropertyReflection> get members => [
    hasBaleen,
hasTeeth,
className,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Whale.canDeserialize(src);
  @override
  Whale Function(Object? src) get deserializeFunction =>
      (src) => Whale.deserialize(src);

  @override
  Object? Function(Whale src) get serializeFunction =>
      (src) => src.serialize();
}

class WhaleXmlReflection {
    const WhaleXmlReflection();
}

