// Model serialization
part of 'array_of_array_of_number_only.dart';


//class serialization

Map<String, dynamic> _$ArrayOfArrayOfNumberOnlyToJson(ArrayOfArrayOfNumberOnly instance) => <String, dynamic>{

};

ArrayOfArrayOfNumberOnly _$ArrayOfArrayOfNumberOnlyFromJson(Map<String, dynamic> src) {
  return ArrayOfArrayOfNumberOnly.$all(

  );
}

XmlElement _$ArrayOfArrayOfNumberOnlyToXml(ArrayOfArrayOfNumberOnly instance) {
  final reflection = ArrayOfArrayOfNumberOnlyXmlReflection.instance;
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

ArrayOfArrayOfNumberOnly _$ArrayOfArrayOfNumberOnlyFromXml(XmlElement src) {
  final reflection = ArrayOfArrayOfNumberOnlyXmlReflection.instance;
  return ArrayOfArrayOfNumberOnly.$all(

  );
}

