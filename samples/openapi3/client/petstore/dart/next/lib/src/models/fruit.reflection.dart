// Model reflection

part of 'fruit.dart';


//class reflection

class FruitReflection extends ClassReflection<Fruit> {
  static const instance = FruitReflection._(
    color: PropertyReflection(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
    ),
  );
  const FruitReflection._({
    required this.color,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> color;

  @override
  List<PropertyReflection> get members => [
    color,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Fruit.canDeserialize(src);
  @override
  Fruit Function(Object? src) get deserializeFunction =>
      (src) => Fruit.deserialize(src);

  @override
  Object? Function(Fruit src) get serializeFunction =>
      (src) => src.serialize();
}

class FruitXmlReflection {
    const FruitXmlReflection();
}

