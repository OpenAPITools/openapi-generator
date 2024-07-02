// Model reflection

part of 'grandparent_animal.dart';


//class reflection

class GrandparentAnimalReflection extends ClassReflection<GrandparentAnimal> {
  static const instance = GrandparentAnimalReflection._(
    petType: PropertyReflection(
      dartName: r'petType',
      nullable: false,
      required: true,
      oasName: r'pet_type',
      oasType: r'string',
      pattern: null,
    ),
  );
  const GrandparentAnimalReflection._({
    required this.petType,
  });

  final PropertyReflection<
            String
> petType;

  @override
  List<PropertyReflection> get members => [
    petType,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => GrandparentAnimal.canDeserialize(src);
  @override
  GrandparentAnimal Function(Object? src) get deserializeFunction =>
      (src) => GrandparentAnimal.deserialize(src);

  @override
  Object? Function(GrandparentAnimal src) get serializeFunction =>
      (src) => src.serialize();
}

class GrandparentAnimalXmlReflection {
    const GrandparentAnimalXmlReflection();
}

