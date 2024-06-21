// Model serialization
part of 'tag.dart';


//class serialization

Map<String, dynamic> _$TagToJson(Tag instance) => <String, dynamic>{

};

Tag _$TagFromJson(Map<String, dynamic> src) {
  return Tag.$all(

  );
}

XmlElement _$TagToXml(Tag instance) {
  final reflection = TagXmlReflection.instance;
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

Tag _$TagFromXml(XmlElement src) {
  final reflection = TagXmlReflection.instance;
  return Tag.$all(

  );
}

