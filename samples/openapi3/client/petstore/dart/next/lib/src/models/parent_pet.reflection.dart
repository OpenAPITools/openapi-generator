// Model reflection

part of 'parent_pet.dart';


//class reflection

class ParentPetReflection extends ClassReflection<ParentPet> {
  static const instance = ParentPetReflection._(
    petType: PropertyReflection(
      dartName: r'petType',
      nullable: false,
      required: true,
      oasName: r'pet_type',
      oasType: r'string',
      pattern: null,
    ),
  );
  const ParentPetReflection._({
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
    (src) => ParentPet.canDeserialize(src);
  @override
  ParentPet Function(Object? src) get deserializeFunction =>
      (src) => ParentPet.deserialize(src);

  @override
  Object? Function(ParentPet src) get serializeFunction =>
      (src) => src.serialize();
}

class ParentPetXmlReflection {
    const ParentPetXmlReflection();
}

