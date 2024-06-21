// Model serialization
part of 'pet_with_required_tags.dart';


//class serialization

Map<String, dynamic> _$PetWithRequiredTagsToJson(PetWithRequiredTags instance) => <String, dynamic>{

};

PetWithRequiredTags _$PetWithRequiredTagsFromJson(Map<String, dynamic> src) {
  return PetWithRequiredTags.$all(

  );
}

XmlElement _$PetWithRequiredTagsToXml(PetWithRequiredTags instance) {
  final reflection = PetWithRequiredTagsXmlReflection.instance;
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

PetWithRequiredTags _$PetWithRequiredTagsFromXml(XmlElement src) {
  final reflection = PetWithRequiredTagsXmlReflection.instance;
  return PetWithRequiredTags.$all(

  );
}

