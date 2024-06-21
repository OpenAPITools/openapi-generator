// Model serialization
part of 'parent_pet.dart';


//class serialization

Map<String, dynamic> _$ParentPetToJson(ParentPet instance) => <String, dynamic>{

};

ParentPet _$ParentPetFromJson(Map<String, dynamic> src) {
  return ParentPet.$all(

  );
}

XmlElement _$ParentPetToXml(ParentPet instance) {
  final reflection = ParentPetXmlReflection.instance;
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

ParentPet _$ParentPetFromXml(XmlElement src) {
  final reflection = ParentPetXmlReflection.instance;
  return ParentPet.$all(

  );
}

