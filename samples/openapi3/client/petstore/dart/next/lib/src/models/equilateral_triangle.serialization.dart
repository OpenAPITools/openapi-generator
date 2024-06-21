// Model serialization
part of 'equilateral_triangle.dart';


//class serialization

Map<String, dynamic> _$EquilateralTriangleToJson(EquilateralTriangle instance) => <String, dynamic>{

};

EquilateralTriangle _$EquilateralTriangleFromJson(Map<String, dynamic> src) {
  return EquilateralTriangle.$all(

  );
}

XmlElement _$EquilateralTriangleToXml(EquilateralTriangle instance) {
  final reflection = EquilateralTriangleXmlReflection.instance;
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

EquilateralTriangle _$EquilateralTriangleFromXml(XmlElement src) {
  final reflection = EquilateralTriangleXmlReflection.instance;
  return EquilateralTriangle.$all(

  );
}

