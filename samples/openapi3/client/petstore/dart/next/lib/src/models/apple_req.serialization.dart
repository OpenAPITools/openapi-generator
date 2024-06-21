// Model serialization
part of 'apple_req.dart';


//class serialization

Map<String, dynamic> _$AppleReqToJson(AppleReq instance) => <String, dynamic>{

};

AppleReq _$AppleReqFromJson(Map<String, dynamic> src) {
  return AppleReq.$all(

  );
}

XmlElement _$AppleReqToXml(AppleReq instance) {
  final reflection = AppleReqXmlReflection.instance;
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

AppleReq _$AppleReqFromXml(XmlElement src) {
  final reflection = AppleReqXmlReflection.instance;
  return AppleReq.$all(

  );
}

