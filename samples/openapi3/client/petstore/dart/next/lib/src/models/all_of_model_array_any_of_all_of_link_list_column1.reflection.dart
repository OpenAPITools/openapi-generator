// Model reflection

part of 'all_of_model_array_any_of_all_of_link_list_column1.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection extends ClassReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1> {
  static const instance = AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection._(
    value: PropertyReflection(
      dartName: r'value',
      nullable: false,
      required: true,
      oasName: r'value',
      oasType: r'array',
      pattern: null,
    ),
  );
  const AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection._({
    required this.value,
  });

  final PropertyReflection<
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
> value;

  @override
  List<PropertyReflection> get members => [
    value,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfLinkListColumn1 Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfLinkListColumn1 src) get serializeFunction =>
      (src) => src.serialize();
}

class AllOfModelArrayAnyOfAllOfLinkListColumn1XmlReflection {
    const AllOfModelArrayAnyOfAllOfLinkListColumn1XmlReflection();
}

