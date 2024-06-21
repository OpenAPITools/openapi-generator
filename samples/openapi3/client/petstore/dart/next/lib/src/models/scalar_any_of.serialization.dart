// Model serialization
part of 'scalar_any_of.dart';


//class serialization

Map<String, dynamic> _$ScalarAnyOfToJson(ScalarAnyOf instance) => <String, dynamic>{

};

ScalarAnyOf _$ScalarAnyOfFromJson(Map<String, dynamic> src) {
  return ScalarAnyOf.$all(

  );
}

XmlElement _$ScalarAnyOfToXml(ScalarAnyOf instance) {
  final reflection = ScalarAnyOfXmlReflection.instance;
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

ScalarAnyOf _$ScalarAnyOfFromXml(XmlElement src) {
  final reflection = ScalarAnyOfXmlReflection.instance;
  return ScalarAnyOf.$all(

  );
}

