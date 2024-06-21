// Model serialization
part of 'banana.dart';


//class serialization

Map<String, dynamic> _$BananaToJson(Banana instance) => <String, dynamic>{

};

Banana _$BananaFromJson(Map<String, dynamic> src) {
  return Banana.$all(

  );
}

XmlElement _$BananaToXml(Banana instance) {
  final reflection = BananaXmlReflection.instance;
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

Banana _$BananaFromXml(XmlElement src) {
  final reflection = BananaXmlReflection.instance;
  return Banana.$all(

  );
}

