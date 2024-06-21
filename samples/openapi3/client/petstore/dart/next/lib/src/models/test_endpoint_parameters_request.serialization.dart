// Model serialization
part of 'test_endpoint_parameters_request.dart';


//class serialization

Map<String, dynamic> _$TestEndpointParametersRequestToJson(TestEndpointParametersRequest instance) => <String, dynamic>{

};

TestEndpointParametersRequest _$TestEndpointParametersRequestFromJson(Map<String, dynamic> src) {
  return TestEndpointParametersRequest.$all(

  );
}

XmlElement _$TestEndpointParametersRequestToXml(TestEndpointParametersRequest instance) {
  final reflection = TestEndpointParametersRequestXmlReflection.instance;
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

TestEndpointParametersRequest _$TestEndpointParametersRequestFromXml(XmlElement src) {
  final reflection = TestEndpointParametersRequestXmlReflection.instance;
  return TestEndpointParametersRequest.$all(

  );
}

