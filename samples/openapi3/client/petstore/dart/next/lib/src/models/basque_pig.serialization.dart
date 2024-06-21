// Model serialization
part of 'basque_pig.dart';


//class serialization

Map<String, dynamic> _$BasquePigToJson(BasquePig instance) => <String, dynamic>{

};

BasquePig _$BasquePigFromJson(Map<String, dynamic> src) {
  return BasquePig.$all(

  );
}

XmlElement _$BasquePigToXml(BasquePig instance) {
  final reflection = BasquePigXmlReflection.instance;
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

BasquePig _$BasquePigFromXml(XmlElement src) {
  final reflection = BasquePigXmlReflection.instance;
  return BasquePig.$all(

  );
}

