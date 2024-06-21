// Model serialization
part of 'category.dart';


//class serialization

Map<String, dynamic> _$CategoryToJson(Category instance) => <String, dynamic>{

};

Category _$CategoryFromJson(Map<String, dynamic> src) {
  return Category.$all(

  );
}

XmlElement _$CategoryToXml(Category instance) {
  final reflection = CategoryXmlReflection.instance;
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

Category _$CategoryFromXml(XmlElement src) {
  final reflection = CategoryXmlReflection.instance;
  return Category.$all(

  );
}

