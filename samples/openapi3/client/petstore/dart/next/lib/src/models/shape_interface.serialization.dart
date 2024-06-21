// Model serialization
part of 'shape_interface.dart';


//class serialization

Map<String, dynamic> _$ShapeInterfaceToJson(ShapeInterface instance) => <String, dynamic>{

};

ShapeInterface _$ShapeInterfaceFromJson(Map<String, dynamic> src) {
  return ShapeInterface.$all(

  );
}

XmlElement _$ShapeInterfaceToXml(ShapeInterface instance) {
  final reflection = ShapeInterfaceXmlReflection.instance;
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

ShapeInterface _$ShapeInterfaceFromXml(XmlElement src) {
  final reflection = ShapeInterfaceXmlReflection.instance;
  return ShapeInterface.$all(

  );
}

