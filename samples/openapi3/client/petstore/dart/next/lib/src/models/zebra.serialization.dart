// Model serialization
part of 'zebra.dart';


//class serialization

Map<String, dynamic> _$ZebraToJson(Zebra instance) => <String, dynamic>{

};

Zebra _$ZebraFromJson(Map<String, dynamic> src) {
  return Zebra.$all(

  );
}

XmlElement _$ZebraToXml(Zebra instance) {
  final reflection = ZebraXmlReflection.instance;
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

Zebra _$ZebraFromXml(XmlElement src) {
  final reflection = ZebraXmlReflection.instance;
  return Zebra.$all(

  );
}

