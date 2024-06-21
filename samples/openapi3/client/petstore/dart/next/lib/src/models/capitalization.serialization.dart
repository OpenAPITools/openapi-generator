// Model serialization
part of 'capitalization.dart';


//class serialization

Map<String, dynamic> _$CapitalizationToJson(Capitalization instance) => <String, dynamic>{

};

Capitalization _$CapitalizationFromJson(Map<String, dynamic> src) {
  return Capitalization.$all(

  );
}

XmlElement _$CapitalizationToXml(Capitalization instance) {
  final reflection = CapitalizationXmlReflection.instance;
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

Capitalization _$CapitalizationFromXml(XmlElement src) {
  final reflection = CapitalizationXmlReflection.instance;
  return Capitalization.$all(

  );
}

