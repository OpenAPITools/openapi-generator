// Model serialization
part of 'pets_multicontent_test_post_request.dart';


//class serialization

Map<String, dynamic> _$PetsMulticontentTestPostRequestToJson(PetsMulticontentTestPostRequest instance) => <String, dynamic>{

};

PetsMulticontentTestPostRequest _$PetsMulticontentTestPostRequestFromJson(Map<String, dynamic> src) {
  return PetsMulticontentTestPostRequest.$all(

  );
}

XmlElement _$PetsMulticontentTestPostRequestToXml(PetsMulticontentTestPostRequest instance) {
  final reflection = PetsMulticontentTestPostRequestXmlReflection.instance;
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

PetsMulticontentTestPostRequest _$PetsMulticontentTestPostRequestFromXml(XmlElement src) {
  final reflection = PetsMulticontentTestPostRequestXmlReflection.instance;
  return PetsMulticontentTestPostRequest.$all(

  );
}

