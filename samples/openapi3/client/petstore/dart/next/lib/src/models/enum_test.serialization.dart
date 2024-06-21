// Model serialization
part of 'enum_test.dart';


//class serialization

Map<String, dynamic> _$EnumTestToJson(EnumTest instance) => <String, dynamic>{

};

EnumTest _$EnumTestFromJson(Map<String, dynamic> src) {
  return EnumTest.$all(

  );
}

XmlElement _$EnumTestToXml(EnumTest instance) {
  final reflection = EnumTestXmlReflection.instance;
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

EnumTest _$EnumTestFromXml(XmlElement src) {
  final reflection = EnumTestXmlReflection.instance;
  return EnumTest.$all(

  );
}

