// Model reflection

part of 'pet_using_all_of.dart';


//class reflection

class PetUsingAllOfReflection extends ClassReflection<PetUsingAllOf> {
  static const instance = PetUsingAllOfReflection._(
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
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
    photoUrls: PropertyReflection(
      dartName: r'photoUrls',
      nullable: false,
      required: true,
      oasName: r'photoUrls',
      oasType: r'array',
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
  const PetUsingAllOfReflection._({
    required this.id,
  
    required this.category,
  
    required this.name,
  
    required this.photoUrls,
  
    required this.tags,
  
    required this.status,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<UndefinedWrapper<
            Category
>> category;
  final PropertyReflection<
            String
> name;
  final PropertyReflection<
    List<
        
            String
>
> photoUrls;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            Tag
>
>> tags;
  final PropertyReflection<UndefinedWrapper<
            PetUsingAllOfStatusEnum
>> status;

  @override
  List<PropertyReflection> get members => [
    id,
category,
name,
photoUrls,
tags,
status,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetUsingAllOf.canDeserialize(src);
  @override
  PetUsingAllOf Function(Object? src) get deserializeFunction =>
      (src) => PetUsingAllOf.deserialize(src);

  @override
  Object? Function(PetUsingAllOf src) get serializeFunction =>
      (src) => src.serialize();
}

class PetUsingAllOfXmlReflection {
    const PetUsingAllOfXmlReflection();
}

