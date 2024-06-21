// Model serialization
part of 'fruit_req.dart';


//class serialization

Map<String, dynamic> _$FruitReqToJson(FruitReq instance) => <String, dynamic>{

};

FruitReq _$FruitReqFromJson(Map<String, dynamic> src) {
  return FruitReq.$all(

  );
}

XmlElement _$FruitReqToXml(FruitReq instance) {
  final reflection = FruitReqXmlReflection.instance;
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

FruitReq _$FruitReqFromXml(XmlElement src) {
  final reflection = FruitReqXmlReflection.instance;
  return FruitReq.$all(

  );
}

