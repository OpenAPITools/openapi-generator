// Model serialization
part of 'shape.dart';


//class serialization

Map<String, dynamic> _$ShapeToJson(Shape instance) => <String, dynamic>{

};

Shape _$ShapeFromJson(Map<String, dynamic> src) {
  return Shape.$all(

  );
}

XmlElement _$ShapeToXml(Shape instance) {
  final reflection = ShapeXmlReflection.instance;
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

Shape _$ShapeFromXml(XmlElement src) {
  final reflection = ShapeXmlReflection.instance;
  return Shape.$all(

  );
}

