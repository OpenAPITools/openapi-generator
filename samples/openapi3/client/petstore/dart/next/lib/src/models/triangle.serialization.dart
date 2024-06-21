// Model serialization
part of 'triangle.dart';


//class serialization

Map<String, dynamic> _$TriangleToJson(Triangle instance) => <String, dynamic>{

};

Triangle _$TriangleFromJson(Map<String, dynamic> src) {
  return Triangle.$all(

  );
}

XmlElement _$TriangleToXml(Triangle instance) {
  final reflection = TriangleXmlReflection.instance;
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

Triangle _$TriangleFromXml(XmlElement src) {
  final reflection = TriangleXmlReflection.instance;
  return Triangle.$all(

  );
}

