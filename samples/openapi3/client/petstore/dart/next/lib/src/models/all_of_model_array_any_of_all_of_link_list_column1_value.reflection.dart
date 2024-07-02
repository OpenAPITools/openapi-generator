// Model reflection

part of 'all_of_model_array_any_of_all_of_link_list_column1_value.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection extends ClassReflection<AllOfModelArrayAnyOfAllOfLinkListColumn1Value> {
  static const instance = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection._(
  );
  const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1Value.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfLinkListColumn1Value Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfLinkListColumn1Value.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfLinkListColumn1Value src) get serializeFunction =>
      (src) => src.serialize();
}

class AllOfModelArrayAnyOfAllOfLinkListColumn1ValueXmlReflection {
    const AllOfModelArrayAnyOfAllOfLinkListColumn1ValueXmlReflection();
}

