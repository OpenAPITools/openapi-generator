// Model serialization
part of 'gm_fruit.dart';


//class serialization

Map<String, dynamic> _$GmFruitToJson(GmFruit instance) => <String, dynamic>{

};

GmFruit _$GmFruitFromJson(Map<String, dynamic> src) {
  return GmFruit.$all(

  );
}

XmlElement _$GmFruitToXml(GmFruit instance) {
  final reflection = GmFruitXmlReflection.instance;
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

GmFruit _$GmFruitFromXml(XmlElement src) {
  final reflection = GmFruitXmlReflection.instance;
  return GmFruit.$all(

  );
}

