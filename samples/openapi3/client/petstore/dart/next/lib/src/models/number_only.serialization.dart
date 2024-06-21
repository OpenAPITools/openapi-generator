// Model serialization
part of 'number_only.dart';


//class serialization

Map<String, dynamic> _$NumberOnlyToJson(NumberOnly instance) => <String, dynamic>{

};

NumberOnly _$NumberOnlyFromJson(Map<String, dynamic> src) {
  return NumberOnly.$all(

  );
}

XmlElement _$NumberOnlyToXml(NumberOnly instance) {
  final reflection = NumberOnlyXmlReflection.instance;
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

NumberOnly _$NumberOnlyFromXml(XmlElement src) {
  final reflection = NumberOnlyXmlReflection.instance;
  return NumberOnly.$all(

  );
}

