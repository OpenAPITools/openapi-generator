// Model reflection

part of 'dog.dart';


//class reflection

class DogReflection extends ClassReflection<Dog> {
  static const instance = DogReflection._(
    color: PropertyReflection(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
    ),
    breed: PropertyReflection(
      dartName: r'breed',
      nullable: false,
      required: false,
      oasName: r'breed',
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
  const DogReflection._({
    required this.color,
  
    required this.breed,
  
    required this.className,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> color;
  final PropertyReflection<UndefinedWrapper<
            String
>> breed;
  final PropertyReflection<
            String
> className;

  @override
  List<PropertyReflection> get members => [
    color,
breed,
className,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Dog.canDeserialize(src);
  @override
  Dog Function(Object? src) get deserializeFunction =>
      (src) => Dog.deserialize(src);

  @override
  Object? Function(Dog src) get serializeFunction =>
      (src) => src.serialize();
}

class DogXmlReflection {
    const DogXmlReflection();
}

