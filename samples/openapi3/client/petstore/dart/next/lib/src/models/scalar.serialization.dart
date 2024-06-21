// Model serialization
part of 'scalar.dart';


//class serialization

Map<String, dynamic> _$ScalarToJson(Scalar instance) => <String, dynamic>{

};

Scalar _$ScalarFromJson(Map<String, dynamic> src) {
  return Scalar.$all(

  );
}

XmlElement _$ScalarToXml(Scalar instance) {
  final reflection = ScalarXmlReflection.instance;
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

Scalar _$ScalarFromXml(XmlElement src) {
  final reflection = ScalarXmlReflection.instance;
  return Scalar.$all(

  );
}

