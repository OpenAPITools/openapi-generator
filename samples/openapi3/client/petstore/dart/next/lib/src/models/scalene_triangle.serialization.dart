// Model serialization
part of 'scalene_triangle.dart';


//class serialization

Map<String, dynamic> _$ScaleneTriangleToJson(ScaleneTriangle instance) => <String, dynamic>{

};

ScaleneTriangle _$ScaleneTriangleFromJson(Map<String, dynamic> src) {
  return ScaleneTriangle.$all(

  );
}

XmlElement _$ScaleneTriangleToXml(ScaleneTriangle instance) {
  final reflection = ScaleneTriangleXmlReflection.instance;
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

ScaleneTriangle _$ScaleneTriangleFromXml(XmlElement src) {
  final reflection = ScaleneTriangleXmlReflection.instance;
  return ScaleneTriangle.$all(

  );
}

