// Model serialization
part of 'pet.dart';


//class serialization

Map<String, dynamic> _$PetToJson(Pet instance) => <String, dynamic>{

};

Pet _$PetFromJson(Map<String, dynamic> src) {
  return Pet.$all(

  );
}

XmlElement _$PetToXml(Pet instance) {
  final reflection = PetXmlReflection.instance;
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

Pet _$PetFromXml(XmlElement src) {
  final reflection = PetXmlReflection.instance;
  return Pet.$all(

  );
}

