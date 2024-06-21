// Model serialization
part of 'cat.dart';


//class serialization

Map<String, dynamic> _$CatToJson(Cat instance) => <String, dynamic>{

};

Cat _$CatFromJson(Map<String, dynamic> src) {
  return Cat.$all(

  );
}

XmlElement _$CatToXml(Cat instance) {
  final reflection = CatXmlReflection.instance;
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

Cat _$CatFromXml(XmlElement src) {
  final reflection = CatXmlReflection.instance;
  return Cat.$all(

  );
}

