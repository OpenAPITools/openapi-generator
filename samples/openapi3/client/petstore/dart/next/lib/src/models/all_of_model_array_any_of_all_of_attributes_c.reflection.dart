// Model reflection

part of 'all_of_model_array_any_of_all_of_attributes_c.dart';


//class reflection

class AllOfModelArrayAnyOfAllOfAttributesCReflection extends ClassReflection<AllOfModelArrayAnyOfAllOfAttributesC> {
  static const instance = AllOfModelArrayAnyOfAllOfAttributesCReflection._(
  );
  const AllOfModelArrayAnyOfAllOfAttributesCReflection._();


  @override
  List<PropertyReflection> get members => [
      ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOfAllOfAttributesC.canDeserialize(src);
  @override
  AllOfModelArrayAnyOfAllOfAttributesC Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOfAllOfAttributesC.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOfAllOfAttributesC src) get serializeFunction =>
      (src) => src.serialize();
}

class AllOfModelArrayAnyOfAllOfAttributesCXmlReflection {
    const AllOfModelArrayAnyOfAllOfAttributesCXmlReflection();
}

