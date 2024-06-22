// Model reflection

part of 'all_of_model_array_any_of_all_of_attributes.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfAttributesReflection extends ClassReflection<AllOfModelArrayAnyOfAllOfAttributes> {
  static const instance = AllOfModelArrayAnyOfAllOfAttributesReflection._(
    C: PropertyReflection(
      dartName: r'C',
      nullable: false,
      required: false,
      oasName: r'C',
      oasType: r'AllOfModelArrayAnyOfAllOfAttributesC',
      pattern: null,
    ),
  );
  const AllOfModelArrayAnyOfAllOfAttributesReflection._({
    required this.C,
  });

  final PropertyReflection<UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributesC
>> C;

  @override
  List<PropertyReflection> get members => [
    C,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfAttributes.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfAttributes Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfAttributes.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfAttributes src) get serializeFunction =>
      (src) => src.serialize();
}

class AllOfModelArrayAnyOfAllOfAttributesXmlReflection {
    const AllOfModelArrayAnyOfAllOfAttributesXmlReflection();
}

