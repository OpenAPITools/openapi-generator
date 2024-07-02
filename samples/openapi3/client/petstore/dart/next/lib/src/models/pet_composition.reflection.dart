// Model reflection

part of 'pet_composition.dart';


//class reflection

class PetCompositionReflection extends ClassReflection<PetComposition> {
  static const instance = PetCompositionReflection._(
    photoUrls: PropertyReflection(
      dartName: r'photoUrls',
      nullable: false,
      required: true,
      oasName: r'photoUrls',
      oasType: r'array',
      pattern: null,
    ),
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
    id: PropertyReflection(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
    ),
    category: PropertyReflection(
      dartName: r'category',
      nullable: false,
      required: false,
      oasName: r'category',
      oasType: r'Category',
      pattern: null,
    ),
    tags: PropertyReflection(
      dartName: r'tags',
      nullable: false,
      required: false,
      oasName: r'tags',
      oasType: r'array',
      pattern: null,
    ),
    status: PropertyReflection(
      dartName: r'status',
      nullable: false,
      required: false,
      oasName: r'status',
      oasType: r'string',
      pattern: null,
    ),
  );
  const PetCompositionReflection._({
    required this.photoUrls,
  
    required this.name,
  
    required this.id,
  
    required this.category,
  
    required this.tags,
  
    required this.status,
  });

  final PropertyReflection<
    List<
        
            String
>
> photoUrls;
  final PropertyReflection<
            String
> name;
  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<UndefinedWrapper<
            Category
>> category;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            Tag
>
>> tags;
  final PropertyReflection<UndefinedWrapper<
            PetStatusEnum
>> status;

  @override
  List<PropertyReflection> get members => [
    photoUrls,
name,
id,
category,
tags,
status,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetComposition.canDeserialize(src);
  @override
  PetComposition Function(Object? src) get deserializeFunction =>
      (src) => PetComposition.deserialize(src);

  @override
  Object? Function(PetComposition src) get serializeFunction =>
      (src) => src.serialize();
}

class PetCompositionXmlReflection {
    const PetCompositionXmlReflection();
}

