// Model serialization
part of 'isosceles_triangle.dart';


//class serialization

Map<String, dynamic> _$IsoscelesTriangleToJson(IsoscelesTriangle instance) => <String, dynamic>{

};

IsoscelesTriangle _$IsoscelesTriangleFromJson(Map<String, dynamic> src) {
  return IsoscelesTriangle.$all(

  );
}

XmlElement _$IsoscelesTriangleToXml(IsoscelesTriangle instance) {
  final reflection = IsoscelesTriangleXmlReflection.instance;
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

IsoscelesTriangle _$IsoscelesTriangleFromXml(XmlElement src) {
  final reflection = IsoscelesTriangleXmlReflection.instance;
  return IsoscelesTriangle.$all(

  );
}

