// Model serialization
part of 'format_test.dart';


//class serialization

Map<String, dynamic> _$FormatTestToJson(FormatTest instance) => <String, dynamic>{

};

FormatTest _$FormatTestFromJson(Map<String, dynamic> src) {
  return FormatTest.$all(

  );
}

XmlElement _$FormatTestToXml(FormatTest instance) {
  final reflection = FormatTestXmlReflection.instance;
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

FormatTest _$FormatTestFromXml(XmlElement src) {
  final reflection = FormatTestXmlReflection.instance;
  return FormatTest.$all(

  );
}

