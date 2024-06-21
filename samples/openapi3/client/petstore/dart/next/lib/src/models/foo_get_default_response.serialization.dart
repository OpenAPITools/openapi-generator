// Model serialization
part of 'foo_get_default_response.dart';


//class serialization

Map<String, dynamic> _$FooGetDefaultResponseToJson(FooGetDefaultResponse instance) => <String, dynamic>{

};

FooGetDefaultResponse _$FooGetDefaultResponseFromJson(Map<String, dynamic> src) {
  return FooGetDefaultResponse.$all(

  );
}

XmlElement _$FooGetDefaultResponseToXml(FooGetDefaultResponse instance) {
  final reflection = FooGetDefaultResponseXmlReflection.instance;
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

FooGetDefaultResponse _$FooGetDefaultResponseFromXml(XmlElement src) {
  final reflection = FooGetDefaultResponseXmlReflection.instance;
  return FooGetDefaultResponse.$all(

  );
}

