// Model serialization
part of 'shape_or_null.dart';


//class serialization

Map<String, dynamic> _$ShapeOrNullToJson(ShapeOrNull instance) => <String, dynamic>{

};

ShapeOrNull _$ShapeOrNullFromJson(Map<String, dynamic> src) {
  return ShapeOrNull.$all(

  );
}

XmlElement _$ShapeOrNullToXml(ShapeOrNull instance) {
  final reflection = ShapeOrNullXmlReflection.instance;
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

ShapeOrNull _$ShapeOrNullFromXml(XmlElement src) {
  final reflection = ShapeOrNullXmlReflection.instance;
  return ShapeOrNull.$all(

  );
}

