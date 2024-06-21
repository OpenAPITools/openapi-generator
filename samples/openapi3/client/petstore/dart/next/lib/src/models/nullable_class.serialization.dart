// Model serialization
part of 'nullable_class.dart';


//class serialization

Map<String, dynamic> _$NullableClassToJson(NullableClass instance) => <String, dynamic>{

};

NullableClass _$NullableClassFromJson(Map<String, dynamic> src) {
  return NullableClass.$all(

  );
}

XmlElement _$NullableClassToXml(NullableClass instance) {
  final reflection = NullableClassXmlReflection.instance;
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

NullableClass _$NullableClassFromXml(XmlElement src) {
  final reflection = NullableClassXmlReflection.instance;
  return NullableClass.$all(

  );
}

