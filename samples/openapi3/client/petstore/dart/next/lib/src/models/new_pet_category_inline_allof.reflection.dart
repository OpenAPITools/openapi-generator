// Model reflection

part of 'new_pet_category_inline_allof.dart';


//class reflection

class NewPetCategoryInlineAllofReflection extends ClassReflection<NewPetCategoryInlineAllof> {
  static const instance = NewPetCategoryInlineAllofReflection._(
    id: PropertyReflection(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
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
    categoryTag: PropertyReflection(
      dartName: r'categoryTag',
      nullable: false,
      required: false,
      oasName: r'category_tag',
      oasType: r'NewPetCategoryInlineAllofAllOfCategoryTag',
      pattern: null,
    ),
  );
  const NewPetCategoryInlineAllofReflection._({
    required this.id,
  
    required this.name,
  
    required this.categoryTag,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<
            String
> name;
  final PropertyReflection<UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
>> categoryTag;

  @override
  List<PropertyReflection> get members => [
    id,
name,
categoryTag,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NewPetCategoryInlineAllof.canDeserialize(src);
  @override
  NewPetCategoryInlineAllof Function(Object? src) get deserializeFunction =>
      (src) => NewPetCategoryInlineAllof.deserialize(src);

  @override
  Object? Function(NewPetCategoryInlineAllof src) get serializeFunction =>
      (src) => src.serialize();
}

class NewPetCategoryInlineAllofXmlReflection {
    const NewPetCategoryInlineAllofXmlReflection();
}

