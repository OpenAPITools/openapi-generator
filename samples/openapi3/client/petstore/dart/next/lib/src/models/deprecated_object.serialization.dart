// Model serialization
part of 'deprecated_object.dart';


//class serialization

Map<String, dynamic> _$DeprecatedObjectToJson(DeprecatedObject instance) => <String, dynamic>{

};

DeprecatedObject _$DeprecatedObjectFromJson(Map<String, dynamic> src) {
  return DeprecatedObject.$all(

  );
}

XmlElement _$DeprecatedObjectToXml(DeprecatedObject instance) {
  final reflection = DeprecatedObjectXmlReflection.instance;
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

DeprecatedObject _$DeprecatedObjectFromXml(XmlElement src) {
  final reflection = DeprecatedObjectXmlReflection.instance;
  return DeprecatedObject.$all(

  );
}

