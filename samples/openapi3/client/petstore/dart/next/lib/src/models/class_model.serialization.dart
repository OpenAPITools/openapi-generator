// Model serialization
part of 'class_model.dart';


//class serialization

Map<String, dynamic> _$ClassModelToJson(ClassModel instance) => <String, dynamic>{

};

ClassModel _$ClassModelFromJson(Map<String, dynamic> src) {
  return ClassModel.$all(

  );
}

XmlElement _$ClassModelToXml(ClassModel instance) {
  final reflection = ClassModelXmlReflection.instance;
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

ClassModel _$ClassModelFromXml(XmlElement src) {
  final reflection = ClassModelXmlReflection.instance;
  return ClassModel.$all(

  );
}

