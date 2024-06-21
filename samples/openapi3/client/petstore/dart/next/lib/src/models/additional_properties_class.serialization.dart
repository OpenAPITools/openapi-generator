// Model serialization
part of 'additional_properties_class.dart';


//class serialization

Map<String, dynamic> _$AdditionalPropertiesClassToJson(AdditionalPropertiesClass instance) => <String, dynamic>{

};

AdditionalPropertiesClass _$AdditionalPropertiesClassFromJson(Map<String, dynamic> src) {
  return AdditionalPropertiesClass.$all(

  );
}

XmlElement _$AdditionalPropertiesClassToXml(AdditionalPropertiesClass instance) {
  final reflection = AdditionalPropertiesClassXmlReflection.instance;
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

AdditionalPropertiesClass _$AdditionalPropertiesClassFromXml(XmlElement src) {
  final reflection = AdditionalPropertiesClassXmlReflection.instance;
  return AdditionalPropertiesClass.$all(

  );
}

