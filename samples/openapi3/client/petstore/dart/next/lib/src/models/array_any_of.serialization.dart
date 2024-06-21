// Model serialization
part of 'array_any_of.dart';


//class serialization

Map<String, dynamic> _$ArrayAnyOfToJson(ArrayAnyOf instance) => <String, dynamic>{

};

ArrayAnyOf _$ArrayAnyOfFromJson(Map<String, dynamic> src) {
  return ArrayAnyOf.$all(

  );
}

XmlElement _$ArrayAnyOfToXml(ArrayAnyOf instance) {
  final reflection = ArrayAnyOfXmlReflection.instance;
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

ArrayAnyOf _$ArrayAnyOfFromXml(XmlElement src) {
  final reflection = ArrayAnyOfXmlReflection.instance;
  return ArrayAnyOf.$all(

  );
}

