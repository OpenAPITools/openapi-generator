// Model reflection

part of 'gm_fruit.dart';


//class reflection

class GmFruitReflection extends ClassReflection<GmFruit> {
  static const instance = GmFruitReflection._(
    color: PropertyReflection(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
    ),
  );
  const GmFruitReflection._({
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
    (src) => GmFruit.canDeserialize(src);
  @override
  GmFruit Function(Object? src) get deserializeFunction =>
      (src) => GmFruit.deserialize(src);

  @override
  Object? Function(GmFruit src) get serializeFunction =>
      (src) => src.serialize();
}

class GmFruitXmlReflection {
    const GmFruitXmlReflection();
}

