// Model serialization
part of 'model_with_one_of_any_of_properties.dart';


//class serialization

Map<String, dynamic> _$ModelWithOneOfAnyOfPropertiesToJson(ModelWithOneOfAnyOfProperties instance) => <String, dynamic>{

};

ModelWithOneOfAnyOfProperties _$ModelWithOneOfAnyOfPropertiesFromJson(Map<String, dynamic> src) {
  return ModelWithOneOfAnyOfProperties.$all(

  );
}

XmlElement _$ModelWithOneOfAnyOfPropertiesToXml(ModelWithOneOfAnyOfProperties instance) {
  final reflection = ModelWithOneOfAnyOfPropertiesXmlReflection.instance;
  final result = XmlElement(
    XmlName(reflection.oasName, reflection.oasNamespace),
    //attributes
    [

    ],
    //elements
    [
    ],
  );
  return result;
}

ModelWithOneOfAnyOfProperties _$ModelWithOneOfAnyOfPropertiesFromXml(XmlElement src) {
  final reflection = ModelWithOneOfAnyOfPropertiesXmlReflection.instance;
  return ModelWithOneOfAnyOfProperties.$all(

  );
}

