// Model serialization
part of 'banana_req.dart';


//class serialization

Map<String, dynamic> _$BananaReqToJson(BananaReq instance) => <String, dynamic>{

};

BananaReq _$BananaReqFromJson(Map<String, dynamic> src) {
  return BananaReq.$all(

  );
}

XmlElement _$BananaReqToXml(BananaReq instance) {
  final reflection = BananaReqXmlReflection.instance;
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

BananaReq _$BananaReqFromXml(XmlElement src) {
  final reflection = BananaReqXmlReflection.instance;
  return BananaReq.$all(

  );
}

