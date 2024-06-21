// Model serialization
part of 'read_only_first.dart';


//class serialization

Map<String, dynamic> _$ReadOnlyFirstToJson(ReadOnlyFirst instance) => <String, dynamic>{

};

ReadOnlyFirst _$ReadOnlyFirstFromJson(Map<String, dynamic> src) {
  return ReadOnlyFirst.$all(

  );
}

XmlElement _$ReadOnlyFirstToXml(ReadOnlyFirst instance) {
  final reflection = ReadOnlyFirstXmlReflection.instance;
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

ReadOnlyFirst _$ReadOnlyFirstFromXml(XmlElement src) {
  final reflection = ReadOnlyFirstXmlReflection.instance;
  return ReadOnlyFirst.$all(

  );
}

