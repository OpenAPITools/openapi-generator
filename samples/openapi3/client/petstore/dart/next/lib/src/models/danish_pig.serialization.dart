// Model serialization
part of 'danish_pig.dart';


//class serialization

Map<String, dynamic> _$DanishPigToJson(DanishPig instance) => <String, dynamic>{

};

DanishPig _$DanishPigFromJson(Map<String, dynamic> src) {
  return DanishPig.$all(

  );
}

XmlElement _$DanishPigToXml(DanishPig instance) {
  final reflection = DanishPigXmlReflection.instance;
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

DanishPig _$DanishPigFromXml(XmlElement src) {
  final reflection = DanishPigXmlReflection.instance;
  return DanishPig.$all(

  );
}

