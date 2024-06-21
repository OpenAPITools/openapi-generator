// Model serialization
part of 'whale.dart';


//class serialization

Map<String, dynamic> _$WhaleToJson(Whale instance) => <String, dynamic>{

};

Whale _$WhaleFromJson(Map<String, dynamic> src) {
  return Whale.$all(

  );
}

XmlElement _$WhaleToXml(Whale instance) {
  final reflection = WhaleXmlReflection.instance;
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

Whale _$WhaleFromXml(XmlElement src) {
  final reflection = WhaleXmlReflection.instance;
  return Whale.$all(

  );
}

