// Model reflection

part of 'category.dart';


//class reflection

class CategoryReflection extends ClassReflection<Category> {
  static const instance = CategoryReflection._(
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
  );
  const CategoryReflection._({
    required this.id,
  
    required this.name,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<
            String
> name;

  @override
  List<PropertyReflection> get members => [
    id,
name,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Category.canDeserialize(src);
  @override
  Category Function(Object? src) get deserializeFunction =>
      (src) => Category.deserialize(src);

  @override
  Object? Function(Category src) get serializeFunction =>
      (src) => src.serialize();
}

class CategoryXmlReflection {
    const CategoryXmlReflection();
}

