// Model serialization
part of 'complex_quadrilateral.dart';


//class serialization

Map<String, dynamic> _$ComplexQuadrilateralToJson(ComplexQuadrilateral instance) => <String, dynamic>{

};

ComplexQuadrilateral _$ComplexQuadrilateralFromJson(Map<String, dynamic> src) {
  return ComplexQuadrilateral.$all(

  );
}

XmlElement _$ComplexQuadrilateralToXml(ComplexQuadrilateral instance) {
  final reflection = ComplexQuadrilateralXmlReflection.instance;
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

ComplexQuadrilateral _$ComplexQuadrilateralFromXml(XmlElement src) {
  final reflection = ComplexQuadrilateralXmlReflection.instance;
  return ComplexQuadrilateral.$all(

  );
}

