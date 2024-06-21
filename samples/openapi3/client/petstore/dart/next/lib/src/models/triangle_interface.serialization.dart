// Model serialization
part of 'triangle_interface.dart';


//class serialization

Map<String, dynamic> _$TriangleInterfaceToJson(TriangleInterface instance) => <String, dynamic>{

};

TriangleInterface _$TriangleInterfaceFromJson(Map<String, dynamic> src) {
  return TriangleInterface.$all(

  );
}

XmlElement _$TriangleInterfaceToXml(TriangleInterface instance) {
  final reflection = TriangleInterfaceXmlReflection.instance;
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

TriangleInterface _$TriangleInterfaceFromXml(XmlElement src) {
  final reflection = TriangleInterfaceXmlReflection.instance;
  return TriangleInterface.$all(

  );
}

