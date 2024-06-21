// Model serialization
part of 'array_of_inline_all_of.dart';


//class serialization

Map<String, dynamic> _$ArrayOfInlineAllOfToJson(ArrayOfInlineAllOf instance) => <String, dynamic>{

};

ArrayOfInlineAllOf _$ArrayOfInlineAllOfFromJson(Map<String, dynamic> src) {
  return ArrayOfInlineAllOf.$all(

  );
}

XmlElement _$ArrayOfInlineAllOfToXml(ArrayOfInlineAllOf instance) {
  final reflection = ArrayOfInlineAllOfXmlReflection.instance;
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

ArrayOfInlineAllOf _$ArrayOfInlineAllOfFromXml(XmlElement src) {
  final reflection = ArrayOfInlineAllOfXmlReflection.instance;
  return ArrayOfInlineAllOf.$all(

  );
}

