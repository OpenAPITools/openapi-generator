// Model serialization
part of 'mammal.dart';


//class serialization

Map<String, dynamic> _$MammalToJson(Mammal instance) => <String, dynamic>{

};

Mammal _$MammalFromJson(Map<String, dynamic> src) {
  return Mammal.$all(

  );
}

XmlElement _$MammalToXml(Mammal instance) {
  final reflection = MammalXmlReflection.instance;
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

Mammal _$MammalFromXml(XmlElement src) {
  final reflection = MammalXmlReflection.instance;
  return Mammal.$all(

  );
}

