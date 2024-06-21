// Model serialization
part of 'enum_arrays.dart';


//class serialization

Map<String, dynamic> _$EnumArraysToJson(EnumArrays instance) => <String, dynamic>{

};

EnumArrays _$EnumArraysFromJson(Map<String, dynamic> src) {
  return EnumArrays.$all(

  );
}

XmlElement _$EnumArraysToXml(EnumArrays instance) {
  final reflection = EnumArraysXmlReflection.instance;
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

EnumArrays _$EnumArraysFromXml(XmlElement src) {
  final reflection = EnumArraysXmlReflection.instance;
  return EnumArrays.$all(

  );
}

