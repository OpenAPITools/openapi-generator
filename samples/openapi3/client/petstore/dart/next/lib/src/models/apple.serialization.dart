// Model serialization
part of 'apple.dart';


//class serialization

Map<String, dynamic> _$AppleToJson(Apple instance) => <String, dynamic>{

};

Apple _$AppleFromJson(Map<String, dynamic> src) {
  return Apple.$all(

  );
}

XmlElement _$AppleToXml(Apple instance) {
  final reflection = AppleXmlReflection.instance;
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

Apple _$AppleFromXml(XmlElement src) {
  final reflection = AppleXmlReflection.instance;
  return Apple.$all(

  );
}

