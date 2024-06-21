// Model serialization
part of 'outer_composite.dart';


//class serialization

Map<String, dynamic> _$OuterCompositeToJson(OuterComposite instance) => <String, dynamic>{

};

OuterComposite _$OuterCompositeFromJson(Map<String, dynamic> src) {
  return OuterComposite.$all(

  );
}

XmlElement _$OuterCompositeToXml(OuterComposite instance) {
  final reflection = OuterCompositeXmlReflection.instance;
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

OuterComposite _$OuterCompositeFromXml(XmlElement src) {
  final reflection = OuterCompositeXmlReflection.instance;
  return OuterComposite.$all(

  );
}

