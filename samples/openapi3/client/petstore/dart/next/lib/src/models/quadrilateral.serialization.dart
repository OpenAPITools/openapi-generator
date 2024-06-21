// Model serialization
part of 'quadrilateral.dart';


//class serialization

Map<String, dynamic> _$QuadrilateralToJson(Quadrilateral instance) => <String, dynamic>{

};

Quadrilateral _$QuadrilateralFromJson(Map<String, dynamic> src) {
  return Quadrilateral.$all(

  );
}

XmlElement _$QuadrilateralToXml(Quadrilateral instance) {
  final reflection = QuadrilateralXmlReflection.instance;
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

Quadrilateral _$QuadrilateralFromXml(XmlElement src) {
  final reflection = QuadrilateralXmlReflection.instance;
  return Quadrilateral.$all(

  );
}

