// Model serialization
part of 'test_enum_parameters_request.dart';


//class serialization

Map<String, dynamic> _$TestEnumParametersRequestToJson(TestEnumParametersRequest instance) => <String, dynamic>{

};

TestEnumParametersRequest _$TestEnumParametersRequestFromJson(Map<String, dynamic> src) {
  return TestEnumParametersRequest.$all(

  );
}

XmlElement _$TestEnumParametersRequestToXml(TestEnumParametersRequest instance) {
  final reflection = TestEnumParametersRequestXmlReflection.instance;
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

TestEnumParametersRequest _$TestEnumParametersRequestFromXml(XmlElement src) {
  final reflection = TestEnumParametersRequestXmlReflection.instance;
  return TestEnumParametersRequest.$all(

  );
}

