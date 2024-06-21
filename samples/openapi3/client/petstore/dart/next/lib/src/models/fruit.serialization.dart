// Model serialization
part of 'fruit.dart';


//class serialization

Map<String, dynamic> _$FruitToJson(Fruit instance) => <String, dynamic>{

};

Fruit _$FruitFromJson(Map<String, dynamic> src) {
  return Fruit.$all(

  );
}

XmlElement _$FruitToXml(Fruit instance) {
  final reflection = FruitXmlReflection.instance;
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

Fruit _$FruitFromXml(XmlElement src) {
  final reflection = FruitXmlReflection.instance;
  return Fruit.$all(

  );
}

