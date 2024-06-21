// Model serialization
part of 'client.dart';


//class serialization

Map<String, dynamic> _$ClientToJson(Client instance) => <String, dynamic>{

};

Client _$ClientFromJson(Map<String, dynamic> src) {
  return Client.$all(

  );
}

XmlElement _$ClientToXml(Client instance) {
  final reflection = ClientXmlReflection.instance;
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

Client _$ClientFromXml(XmlElement src) {
  final reflection = ClientXmlReflection.instance;
  return Client.$all(

  );
}

