// Model serialization
part of 'dog.dart';


//class serialization

Map<String, dynamic> _$DogToJson(Dog instance) => <String, dynamic>{

};

Dog _$DogFromJson(Map<String, dynamic> src) {
  return Dog.$all(

  );
}

XmlElement _$DogToXml(Dog instance) {
  final reflection = DogXmlReflection.instance;
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

Dog _$DogFromXml(XmlElement src) {
  final reflection = DogXmlReflection.instance;
  return Dog.$all(

  );
}

