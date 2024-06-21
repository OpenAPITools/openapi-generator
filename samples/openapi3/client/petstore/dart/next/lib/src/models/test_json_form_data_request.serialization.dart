// Model serialization
part of 'test_json_form_data_request.dart';


//class serialization

Map<String, dynamic> _$TestJsonFormDataRequestToJson(TestJsonFormDataRequest instance) => <String, dynamic>{

};

TestJsonFormDataRequest _$TestJsonFormDataRequestFromJson(Map<String, dynamic> src) {
  return TestJsonFormDataRequest.$all(

  );
}

XmlElement _$TestJsonFormDataRequestToXml(TestJsonFormDataRequest instance) {
  final reflection = TestJsonFormDataRequestXmlReflection.instance;
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

TestJsonFormDataRequest _$TestJsonFormDataRequestFromXml(XmlElement src) {
  final reflection = TestJsonFormDataRequestXmlReflection.instance;
  return TestJsonFormDataRequest.$all(

  );
}

