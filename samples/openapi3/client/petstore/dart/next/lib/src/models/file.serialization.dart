// Model serialization
part of 'file.dart';


//class serialization

Map<String, dynamic> _$FileToJson(File instance) => <String, dynamic>{

};

File _$FileFromJson(Map<String, dynamic> src) {
  return File.$all(

  );
}

XmlElement _$FileToXml(File instance) {
  final reflection = FileXmlReflection.instance;
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

File _$FileFromXml(XmlElement src) {
  final reflection = FileXmlReflection.instance;
  return File.$all(

  );
}

