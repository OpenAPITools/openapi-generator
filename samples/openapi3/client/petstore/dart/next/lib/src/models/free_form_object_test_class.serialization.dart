// Model serialization
part of 'free_form_object_test_class.dart';


//class serialization

Map<String, dynamic> _$FreeFormObjectTestClassToJson(FreeFormObjectTestClass instance) => <String, dynamic>{

};

FreeFormObjectTestClass _$FreeFormObjectTestClassFromJson(Map<String, dynamic> src) {
  return FreeFormObjectTestClass.$all(

  );
}

XmlElement _$FreeFormObjectTestClassToXml(FreeFormObjectTestClass instance) {
  final reflection = FreeFormObjectTestClassXmlReflection.instance;
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

FreeFormObjectTestClass _$FreeFormObjectTestClassFromXml(XmlElement src) {
  final reflection = FreeFormObjectTestClassXmlReflection.instance;
  return FreeFormObjectTestClass.$all(

  );
}

