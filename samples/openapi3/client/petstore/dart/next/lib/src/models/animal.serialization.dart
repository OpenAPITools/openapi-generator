// Model serialization
part of 'animal.dart';


//class serialization

Map<String, dynamic> _$AnimalToJson(Animal instance) => <String, dynamic>{

};

Animal _$AnimalFromJson(Map<String, dynamic> src) {
  return Animal.$all(

  );
}

XmlElement _$AnimalToXml(Animal instance) {
  final reflection = AnimalXmlReflection.instance;
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

Animal _$AnimalFromXml(XmlElement src) {
  final reflection = AnimalXmlReflection.instance;
  return Animal.$all(

  );
}

