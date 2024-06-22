// Model reflection

part of 'animal.dart';


//class reflection

class AnimalReflection extends ClassReflection<Animal> {
  static const instance = AnimalReflection._(
    className: PropertyReflection(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
    ),
    color: PropertyReflection(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
    ),
  );
  const AnimalReflection._({
    required this.className,
  
    required this.color,
  });

  final PropertyReflection<
            String
> className;
  final PropertyReflection<UndefinedWrapper<
            String
>> color;

  @override
  List<PropertyReflection> get members => [
    className,
color,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Animal.canDeserialize(src);
  @override
  Animal Function(Object? src) get deserializeFunction =>
      (src) => Animal.deserialize(src);

  @override
  Object? Function(Animal src) get serializeFunction =>
      (src) => src.serialize();
}

class AnimalXmlReflection {
    const AnimalXmlReflection();
}

