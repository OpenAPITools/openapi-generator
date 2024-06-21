// Model serialization
part of 'has_only_read_only.dart';


//class serialization

Map<String, dynamic> _$HasOnlyReadOnlyToJson(HasOnlyReadOnly instance) => <String, dynamic>{

};

HasOnlyReadOnly _$HasOnlyReadOnlyFromJson(Map<String, dynamic> src) {
  return HasOnlyReadOnly.$all(

  );
}

XmlElement _$HasOnlyReadOnlyToXml(HasOnlyReadOnly instance) {
  final reflection = HasOnlyReadOnlyXmlReflection.instance;
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

HasOnlyReadOnly _$HasOnlyReadOnlyFromXml(XmlElement src) {
  final reflection = HasOnlyReadOnlyXmlReflection.instance;
  return HasOnlyReadOnly.$all(

  );
}

