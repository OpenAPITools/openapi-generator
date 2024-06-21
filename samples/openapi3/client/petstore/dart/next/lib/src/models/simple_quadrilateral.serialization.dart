// Model serialization
part of 'simple_quadrilateral.dart';


//class serialization

Map<String, dynamic> _$SimpleQuadrilateralToJson(SimpleQuadrilateral instance) => <String, dynamic>{

};

SimpleQuadrilateral _$SimpleQuadrilateralFromJson(Map<String, dynamic> src) {
  return SimpleQuadrilateral.$all(

  );
}

XmlElement _$SimpleQuadrilateralToXml(SimpleQuadrilateral instance) {
  final reflection = SimpleQuadrilateralXmlReflection.instance;
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

SimpleQuadrilateral _$SimpleQuadrilateralFromXml(XmlElement src) {
  final reflection = SimpleQuadrilateralXmlReflection.instance;
  return SimpleQuadrilateral.$all(

  );
}

