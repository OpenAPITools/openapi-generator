// Model reflection

part of 'banana.dart';


//class reflection

class BananaReflection extends ClassReflection<Banana> {
  static const instance = BananaReflection._(
    lengthCm: PropertyReflection(
      dartName: r'lengthCm',
      nullable: false,
      required: false,
      oasName: r'lengthCm',
      oasType: r'number',
      pattern: null,
    ),
  );
  const BananaReflection._({
    required this.lengthCm,
  });

  final PropertyReflection<UndefinedWrapper<
            num
>> lengthCm;

  @override
  List<PropertyReflection> get members => [
    lengthCm,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Banana.canDeserialize(src);
  @override
  Banana Function(Object? src) get deserializeFunction =>
      (src) => Banana.deserialize(src);

  @override
  Object? Function(Banana src) get serializeFunction =>
      (src) => src.serialize();
}

class BananaXmlReflection {
    const BananaXmlReflection();
}

