// Model serialization
part of 'array_of_number_only.dart';


//class serialization

Map<String, dynamic> _$ArrayOfNumberOnlyToJson(ArrayOfNumberOnly instance) => <String, dynamic>{

};

ArrayOfNumberOnly _$ArrayOfNumberOnlyFromJson(Map<String, dynamic> src) {
  return ArrayOfNumberOnly.$all(

  );
}

XmlElement _$ArrayOfNumberOnlyToXml(ArrayOfNumberOnly instance) {
  final reflection = ArrayOfNumberOnlyXmlReflection.instance;
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

ArrayOfNumberOnly _$ArrayOfNumberOnlyFromXml(XmlElement src) {
  final reflection = ArrayOfNumberOnlyXmlReflection.instance;
  return ArrayOfNumberOnly.$all(

  );
}

