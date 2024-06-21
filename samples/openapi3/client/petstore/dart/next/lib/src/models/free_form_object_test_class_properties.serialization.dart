// Model serialization
part of 'free_form_object_test_class_properties.dart';


//class serialization

Map<String, dynamic> _$FreeFormObjectTestClassPropertiesToJson(FreeFormObjectTestClassProperties instance) => <String, dynamic>{

};

FreeFormObjectTestClassProperties _$FreeFormObjectTestClassPropertiesFromJson(Map<String, dynamic> src) {
  return FreeFormObjectTestClassProperties.$all(

  );
}

XmlElement _$FreeFormObjectTestClassPropertiesToXml(FreeFormObjectTestClassProperties instance) {
  final reflection = FreeFormObjectTestClassPropertiesXmlReflection.instance;
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

FreeFormObjectTestClassProperties _$FreeFormObjectTestClassPropertiesFromXml(XmlElement src) {
  final reflection = FreeFormObjectTestClassPropertiesXmlReflection.instance;
  return FreeFormObjectTestClassProperties.$all(

  );
}

