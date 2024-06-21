// Model serialization
part of 'pets_multicontent_test_post_request_address.dart';


//class serialization

Map<String, dynamic> _$PetsMulticontentTestPostRequestAddressToJson(PetsMulticontentTestPostRequestAddress instance) => <String, dynamic>{

};

PetsMulticontentTestPostRequestAddress _$PetsMulticontentTestPostRequestAddressFromJson(Map<String, dynamic> src) {
  return PetsMulticontentTestPostRequestAddress.$all(

  );
}

XmlElement _$PetsMulticontentTestPostRequestAddressToXml(PetsMulticontentTestPostRequestAddress instance) {
  final reflection = PetsMulticontentTestPostRequestAddressXmlReflection.instance;
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

PetsMulticontentTestPostRequestAddress _$PetsMulticontentTestPostRequestAddressFromXml(XmlElement src) {
  final reflection = PetsMulticontentTestPostRequestAddressXmlReflection.instance;
  return PetsMulticontentTestPostRequestAddress.$all(

  );
}

