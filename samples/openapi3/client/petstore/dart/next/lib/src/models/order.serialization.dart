// Model serialization
part of 'order.dart';


//class serialization

Map<String, dynamic> _$OrderToJson(Order instance) => <String, dynamic>{

};

Order _$OrderFromJson(Map<String, dynamic> src) {
  return Order.$all(

  );
}

XmlElement _$OrderToXml(Order instance) {
  final reflection = OrderXmlReflection.instance;
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

Order _$OrderFromXml(XmlElement src) {
  final reflection = OrderXmlReflection.instance;
  return Order.$all(

  );
}

