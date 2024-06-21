// Model serialization
part of 'array_default.dart';


//class serialization

Map<String, dynamic> _$ArrayDefaultToJson(ArrayDefault instance) => <String, dynamic>{

};

ArrayDefault _$ArrayDefaultFromJson(Map<String, dynamic> src) {
  return ArrayDefault.$all(

  );
}

XmlElement _$ArrayDefaultToXml(ArrayDefault instance) {
  final reflection = ArrayDefaultXmlReflection.instance;
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

ArrayDefault _$ArrayDefaultFromXml(XmlElement src) {
  final reflection = ArrayDefaultXmlReflection.instance;
  return ArrayDefault.$all(

  );
}

