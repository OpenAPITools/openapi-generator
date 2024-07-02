// Model reflection

part of 'zebra.dart';


//class reflection

class ZebraReflection extends ClassReflection<Zebra> {
  static const instance = ZebraReflection._(
    type: PropertyReflection(
      dartName: r'type',
      nullable: false,
      required: false,
      oasName: r'type',
      oasType: r'string',
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
  const ZebraReflection._({
    required this.type,
  
    required this.className,
  });

  final PropertyReflection<UndefinedWrapper<
            ZebraTypeEnum
>> type;
  final PropertyReflection<
            String
> className;

  @override
  List<PropertyReflection> get members => [
    type,
className,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Zebra.canDeserialize(src);
  @override
  Zebra Function(Object? src) get deserializeFunction =>
      (src) => Zebra.deserialize(src);

  @override
  Object? Function(Zebra src) get serializeFunction =>
      (src) => src.serialize();
}

class ZebraXmlReflection {
    const ZebraXmlReflection();
}

