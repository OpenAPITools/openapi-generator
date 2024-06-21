// Model serialization
part of 'pet_composition.dart';


//class serialization

Map<String, dynamic> _$PetCompositionToJson(PetComposition instance) => <String, dynamic>{

};

PetComposition _$PetCompositionFromJson(Map<String, dynamic> src) {
  return PetComposition.$all(

  );
}

XmlElement _$PetCompositionToXml(PetComposition instance) {
  final reflection = PetCompositionXmlReflection.instance;
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

PetComposition _$PetCompositionFromXml(XmlElement src) {
  final reflection = PetCompositionXmlReflection.instance;
  return PetComposition.$all(

  );
}

