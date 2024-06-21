// Model serialization
part of 'special_model_name.dart';


//class serialization

Map<String, dynamic> _$SpecialModelNameToJson(SpecialModelName instance) => <String, dynamic>{

};

SpecialModelName _$SpecialModelNameFromJson(Map<String, dynamic> src) {
  return SpecialModelName.$all(

  );
}

XmlElement _$SpecialModelNameToXml(SpecialModelName instance) {
  final reflection = SpecialModelNameXmlReflection.instance;
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

SpecialModelName _$SpecialModelNameFromXml(XmlElement src) {
  final reflection = SpecialModelNameXmlReflection.instance;
  return SpecialModelName.$all(

  );
}

