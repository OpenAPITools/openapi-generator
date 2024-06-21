// Model serialization
part of 'quadrilateral_interface.dart';


//class serialization

Map<String, dynamic> _$QuadrilateralInterfaceToJson(QuadrilateralInterface instance) => <String, dynamic>{

};

QuadrilateralInterface _$QuadrilateralInterfaceFromJson(Map<String, dynamic> src) {
  return QuadrilateralInterface.$all(

  );
}

XmlElement _$QuadrilateralInterfaceToXml(QuadrilateralInterface instance) {
  final reflection = QuadrilateralInterfaceXmlReflection.instance;
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

QuadrilateralInterface _$QuadrilateralInterfaceFromXml(XmlElement src) {
  final reflection = QuadrilateralInterfaceXmlReflection.instance;
  return QuadrilateralInterface.$all(

  );
}

