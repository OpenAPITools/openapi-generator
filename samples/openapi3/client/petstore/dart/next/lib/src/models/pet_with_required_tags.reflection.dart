// Model reflection

part of 'pet_with_required_tags.dart';


//class reflection

class PetWithRequiredTagsReflection extends ClassReflection<PetWithRequiredTags> {
  static const instance = PetWithRequiredTagsReflection._(
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
      required: true,
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
  const PetWithRequiredTagsReflection._({
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
  final PropertyReflection<
    List<
        
            Tag
>
> tags;
  final PropertyReflection<UndefinedWrapper<
            PetWithRequiredTagsStatusEnum
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
    (src) => PetWithRequiredTags.canDeserialize(src);
  @override
  PetWithRequiredTags Function(Object? src) get deserializeFunction =>
      (src) => PetWithRequiredTags.deserialize(src);

  @override
  Object? Function(PetWithRequiredTags src) get serializeFunction =>
      (src) => src.serialize();
}

class PetWithRequiredTagsXmlReflection {
    const PetWithRequiredTagsXmlReflection();
}

