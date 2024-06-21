// Model serialization
part of 'object_with_deprecated_fields.dart';


//class serialization

Map<String, dynamic> _$ObjectWithDeprecatedFieldsToJson(ObjectWithDeprecatedFields instance) => <String, dynamic>{

};

ObjectWithDeprecatedFields _$ObjectWithDeprecatedFieldsFromJson(Map<String, dynamic> src) {
  return ObjectWithDeprecatedFields.$all(

  );
}

XmlElement _$ObjectWithDeprecatedFieldsToXml(ObjectWithDeprecatedFields instance) {
  final reflection = ObjectWithDeprecatedFieldsXmlReflection.instance;
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

ObjectWithDeprecatedFields _$ObjectWithDeprecatedFieldsFromXml(XmlElement src) {
  final reflection = ObjectWithDeprecatedFieldsXmlReflection.instance;
  return ObjectWithDeprecatedFields.$all(

  );
}

