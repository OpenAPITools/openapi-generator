// Model serialization
part of 'foo.dart';


//class serialization

Map<String, dynamic> _$FooToJson(Foo instance) => <String, dynamic>{

};

Foo _$FooFromJson(Map<String, dynamic> src) {
  return Foo.$all(

  );
}

XmlElement _$FooToXml(Foo instance) {
  final reflection = FooXmlReflection.instance;
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

Foo _$FooFromXml(XmlElement src) {
  final reflection = FooXmlReflection.instance;
  return Foo.$all(

  );
}

