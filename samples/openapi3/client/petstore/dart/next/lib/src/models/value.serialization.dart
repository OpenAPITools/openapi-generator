// Model serialization
part of 'value.dart';


//class serialization

Map<String, dynamic> _$ValueToJson(Value instance) => <String, dynamic>{

};

Value _$ValueFromJson(Map<String, dynamic> src) {
  return Value.$all(

  );
}

XmlElement _$ValueToXml(Value instance) {
  final reflection = ValueXmlReflection.instance;
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

Value _$ValueFromXml(XmlElement src) {
  final reflection = ValueXmlReflection.instance;
  return Value.$all(

  );
}

