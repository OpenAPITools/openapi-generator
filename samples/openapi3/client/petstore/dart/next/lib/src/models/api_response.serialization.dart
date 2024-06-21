// Model serialization
part of 'api_response.dart';


//class serialization

Map<String, dynamic> _$ApiResponseToJson(ApiResponse instance) => <String, dynamic>{

};

ApiResponse _$ApiResponseFromJson(Map<String, dynamic> src) {
  return ApiResponse.$all(

  );
}

XmlElement _$ApiResponseToXml(ApiResponse instance) {
  final reflection = ApiResponseXmlReflection.instance;
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

ApiResponse _$ApiResponseFromXml(XmlElement src) {
  final reflection = ApiResponseXmlReflection.instance;
  return ApiResponse.$all(

  );
}

