// Model serialization
part of 'array_one_of.dart';


//class serialization

Map<String, dynamic> _$ArrayOneOfToJson(ArrayOneOf instance) => <String, dynamic>{

};

ArrayOneOf _$ArrayOneOfFromJson(Map<String, dynamic> src) {
  return ArrayOneOf.$all(

  );
}

XmlElement _$ArrayOneOfToXml(ArrayOneOf instance) {
  final reflection = ArrayOneOfXmlReflection.instance;
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

ArrayOneOf _$ArrayOneOfFromXml(XmlElement src) {
  final reflection = ArrayOneOfXmlReflection.instance;
  return ArrayOneOf.$all(

  );
}

