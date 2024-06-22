// Model reflection

part of 'new_pet.dart';


//class reflection

class NewPetReflection extends ClassReflection<NewPet> {
  static const instance = NewPetReflection._(
    id: PropertyReflection(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
    ),
    categoryInlineAllof: PropertyReflection(
      dartName: r'categoryInlineAllof',
      nullable: false,
      required: false,
      oasName: r'category_inline_allof',
      oasType: r'NewPetCategoryInlineAllof',
      pattern: null,
    ),
    categoryAllOfRef: PropertyReflection(
      dartName: r'categoryAllOfRef',
      nullable: false,
      required: false,
      oasName: r'category_allOf_ref',
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
  const NewPetReflection._({
    required this.id,
  
    required this.categoryInlineAllof,
  
    required this.categoryAllOfRef,
  
    required this.name,
  
    required this.photoUrls,
  
    required this.tags,
  
    required this.status,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<UndefinedWrapper<
            NewPetCategoryInlineAllof
>> categoryInlineAllof;
  final PropertyReflection<UndefinedWrapper<
            Category
>> categoryAllOfRef;
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
            NewPetStatusEnum
>> status;

  @override
  List<PropertyReflection> get members => [
    id,
categoryInlineAllof,
categoryAllOfRef,
name,
photoUrls,
tags,
status,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NewPet.canDeserialize(src);
  @override
  NewPet Function(Object? src) get deserializeFunction =>
      (src) => NewPet.deserialize(src);

  @override
  Object? Function(NewPet src) get serializeFunction =>
      (src) => src.serialize();
}

class NewPetXmlReflection {
    const NewPetXmlReflection();
}

