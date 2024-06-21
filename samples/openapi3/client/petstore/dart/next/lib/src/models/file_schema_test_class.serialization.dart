// Model serialization
part of 'file_schema_test_class.dart';


//class serialization

Map<String, dynamic> _$FileSchemaTestClassToJson(FileSchemaTestClass instance) => <String, dynamic>{

};

FileSchemaTestClass _$FileSchemaTestClassFromJson(Map<String, dynamic> src) {
  return FileSchemaTestClass.$all(

  );
}

XmlElement _$FileSchemaTestClassToXml(FileSchemaTestClass instance) {
  final reflection = FileSchemaTestClassXmlReflection.instance;
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

FileSchemaTestClass _$FileSchemaTestClassFromXml(XmlElement src) {
  final reflection = FileSchemaTestClassXmlReflection.instance;
  return FileSchemaTestClass.$all(

  );
}

