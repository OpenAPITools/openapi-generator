// Model reflection

part of 'apple.dart';


//class reflection

class AppleReflection extends ClassReflection<Apple> {
  static const instance = AppleReflection._(
    cultivar: PropertyReflection(
      dartName: r'cultivar',
      nullable: false,
      required: false,
      oasName: r'cultivar',
      oasType: r'string',
      pattern: r'/^[a-zA-Z\\s]*$/',
    ),
    origin: PropertyReflection(
      dartName: r'origin',
      nullable: false,
      required: false,
      oasName: r'origin',
      oasType: r'string',
      pattern: r'/^[A-Z\\s]*$/i',
    ),
  );
  const AppleReflection._({
    required this.cultivar,
  
    required this.origin,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> cultivar;
  final PropertyReflection<UndefinedWrapper<
            String
>> origin;

  @override
  List<PropertyReflection> get members => [
    cultivar,
origin,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Apple.canDeserialize(src);
  @override
  Apple Function(Object? src) get deserializeFunction =>
      (src) => Apple.deserialize(src);

  @override
  Object? Function(Apple src) get serializeFunction =>
      (src) => src.serialize();
}

class AppleXmlReflection {
    const AppleXmlReflection();
}

