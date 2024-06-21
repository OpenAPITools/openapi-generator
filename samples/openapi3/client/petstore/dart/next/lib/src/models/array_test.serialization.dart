// Model serialization
part of 'array_test.dart';


//class serialization

Map<String, dynamic> _$ArrayTestToJson(ArrayTest instance) => <String, dynamic>{

};

ArrayTest _$ArrayTestFromJson(Map<String, dynamic> src) {
  return ArrayTest.$all(

  );
}

XmlElement _$ArrayTestToXml(ArrayTest instance) {
  final reflection = ArrayTestXmlReflection.instance;
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

ArrayTest _$ArrayTestFromXml(XmlElement src) {
  final reflection = ArrayTestXmlReflection.instance;
  return ArrayTest.$all(

  );
}

