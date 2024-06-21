// Model serialization
part of 'grandparent_animal.dart';


//class serialization

Map<String, dynamic> _$GrandparentAnimalToJson(GrandparentAnimal instance) => <String, dynamic>{

};

GrandparentAnimal _$GrandparentAnimalFromJson(Map<String, dynamic> src) {
  return GrandparentAnimal.$all(

  );
}

XmlElement _$GrandparentAnimalToXml(GrandparentAnimal instance) {
  final reflection = GrandparentAnimalXmlReflection.instance;
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

GrandparentAnimal _$GrandparentAnimalFromXml(XmlElement src) {
  final reflection = GrandparentAnimalXmlReflection.instance;
  return GrandparentAnimal.$all(

  );
}

