// Model serialization
part of 'pig.dart';


//class serialization

Map<String, dynamic> _$PigToJson(Pig instance) => <String, dynamic>{

};

Pig _$PigFromJson(Map<String, dynamic> src) {
  return Pig.$all(

  );
}

XmlElement _$PigToXml(Pig instance) {
  final reflection = PigXmlReflection.instance;
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

Pig _$PigFromXml(XmlElement src) {
  final reflection = PigXmlReflection.instance;
  return Pig.$all(

  );
}

