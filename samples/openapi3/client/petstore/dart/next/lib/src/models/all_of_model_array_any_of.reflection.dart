// Model reflection

part of 'all_of_model_array_any_of.dart';


//class reflection

class AllOfModelArrayAnyOfReflection extends ClassReflection<AllOfModelArrayAnyOf> {
  static const instance = AllOfModelArrayAnyOfReflection._(
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
    attributes: PropertyReflection(
      dartName: r'attributes',
      nullable: false,
      required: false,
      oasName: r'attributes',
      oasType: r'AllOfModelArrayAnyOfAllOfAttributes',
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
    linkListColumn1: PropertyReflection(
      dartName: r'linkListColumn1',
      nullable: false,
      required: false,
      oasName: r'linkListColumn1',
      oasType: r'AllOfModelArrayAnyOfAllOfLinkListColumn1',
      pattern: null,
    ),
  );
  const AllOfModelArrayAnyOfReflection._({
    required this.name,
  
    required this.attributes,
  
    required this.id,
  
    required this.linkListColumn1,
  });

  final PropertyReflection<
            String
> name;
  final PropertyReflection<UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
>> attributes;
  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
>> linkListColumn1;

  @override
  List<PropertyReflection> get members => [
    name,
attributes,
id,
linkListColumn1,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOf.canDeserialize(src);
  @override
  AllOfModelArrayAnyOf Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOf.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOf src) get serializeFunction =>
      (src) => src.serialize();
}

class AllOfModelArrayAnyOfXmlReflection {
    const AllOfModelArrayAnyOfXmlReflection();
}

