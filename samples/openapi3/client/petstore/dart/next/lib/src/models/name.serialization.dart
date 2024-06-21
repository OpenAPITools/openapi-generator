// Model serialization
part of 'name.dart';


//class serialization

Map<String, dynamic> _$NameToJson(Name instance) => <String, dynamic>{

};

Name _$NameFromJson(Map<String, dynamic> src) {
  return Name.$all(

  );
}

XmlElement _$NameToXml(Name instance) {
  final reflection = NameXmlReflection.instance;
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

Name _$NameFromXml(XmlElement src) {
  final reflection = NameXmlReflection.instance;
  return Name.$all(

  );
}

