// Model serialization
part of 'new_pet.dart';


//class serialization

Map<String, dynamic> _$NewPetToJson(NewPet instance) => <String, dynamic>{

};

NewPet _$NewPetFromJson(Map<String, dynamic> src) {
  return NewPet.$all(

  );
}

XmlElement _$NewPetToXml(NewPet instance) {
  final reflection = NewPetXmlReflection.instance;
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

NewPet _$NewPetFromXml(XmlElement src) {
  final reflection = NewPetXmlReflection.instance;
  return NewPet.$all(

  );
}

