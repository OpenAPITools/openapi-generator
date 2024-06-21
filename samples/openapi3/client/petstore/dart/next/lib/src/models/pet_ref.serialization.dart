// Model serialization
part of 'pet_ref.dart';


//class serialization

Map<String, dynamic> _$PetRefToJson(PetRef instance) => <String, dynamic>{

};

PetRef _$PetRefFromJson(Map<String, dynamic> src) {
  return PetRef.$all(

  );
}

XmlElement _$PetRefToXml(PetRef instance) {
  final reflection = PetRefXmlReflection.instance;
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

PetRef _$PetRefFromXml(XmlElement src) {
  final reflection = PetRefXmlReflection.instance;
  return PetRef.$all(

  );
}

