// Model reflection

part of 'cat.dart';


//class reflection

class CatReflection extends ClassReflection<Cat> {
  static const instance = CatReflection._(
    color: PropertyReflection(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
    ),
    declawed: PropertyReflection(
      dartName: r'declawed',
      nullable: false,
      required: false,
      oasName: r'declawed',
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
  const CatReflection._({
    required this.color,
  
    required this.declawed,
  
    required this.className,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> color;
  final PropertyReflection<UndefinedWrapper<
            bool
>> declawed;
  final PropertyReflection<
            String
> className;

  @override
  List<PropertyReflection> get members => [
    color,
declawed,
className,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Cat.canDeserialize(src);
  @override
  Cat Function(Object? src) get deserializeFunction =>
      (src) => Cat.deserialize(src);

  @override
  Object? Function(Cat src) get serializeFunction =>
      (src) => src.serialize();
}

class CatXmlReflection {
    const CatXmlReflection();
}

