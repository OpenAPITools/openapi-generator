// Model serialization
part of 'update_pet_with_form_request.dart';


//class serialization

Map<String, dynamic> _$UpdatePetWithFormRequestToJson(UpdatePetWithFormRequest instance) => <String, dynamic>{

};

UpdatePetWithFormRequest _$UpdatePetWithFormRequestFromJson(Map<String, dynamic> src) {
  return UpdatePetWithFormRequest.$all(

  );
}

XmlElement _$UpdatePetWithFormRequestToXml(UpdatePetWithFormRequest instance) {
  final reflection = UpdatePetWithFormRequestXmlReflection.instance;
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

UpdatePetWithFormRequest _$UpdatePetWithFormRequestFromXml(XmlElement src) {
  final reflection = UpdatePetWithFormRequestXmlReflection.instance;
  return UpdatePetWithFormRequest.$all(

  );
}

