// Model serialization
part of 'pet_using_all_of.dart';


//class serialization

Map<String, dynamic> _$PetUsingAllOfToJson(PetUsingAllOf instance) => <String, dynamic>{

};

PetUsingAllOf _$PetUsingAllOfFromJson(Map<String, dynamic> src) {
  return PetUsingAllOf.$all(

  );
}

XmlElement _$PetUsingAllOfToXml(PetUsingAllOf instance) {
  final reflection = PetUsingAllOfXmlReflection.instance;
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

PetUsingAllOf _$PetUsingAllOfFromXml(XmlElement src) {
  final reflection = PetUsingAllOfXmlReflection.instance;
  return PetUsingAllOf.$all(

  );
}

