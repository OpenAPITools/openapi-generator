// Model reflection

part of 'new_pet_category_inline_allof_all_of_category_tag.dart';


//class reflection

class NewPetCategoryInlineAllofAllOfCategoryTagReflection extends ClassReflection<NewPetCategoryInlineAllofAllOfCategoryTag> {
  static const instance = NewPetCategoryInlineAllofAllOfCategoryTagReflection._(
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
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
  );
  const NewPetCategoryInlineAllofAllOfCategoryTagReflection._({
    required this.id,
  
    required this.name,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<UndefinedWrapper<
            String
>> name;

  @override
  List<PropertyReflection> get members => [
    id,
name,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NewPetCategoryInlineAllofAllOfCategoryTag.canDeserialize(src);
  @override
  NewPetCategoryInlineAllofAllOfCategoryTag Function(Object? src) get deserializeFunction =>
      (src) => NewPetCategoryInlineAllofAllOfCategoryTag.deserialize(src);

  @override
  Object? Function(NewPetCategoryInlineAllofAllOfCategoryTag src) get serializeFunction =>
      (src) => src.serialize();
}

class NewPetCategoryInlineAllofAllOfCategoryTagXmlReflection {
    const NewPetCategoryInlineAllofAllOfCategoryTagXmlReflection();
}

